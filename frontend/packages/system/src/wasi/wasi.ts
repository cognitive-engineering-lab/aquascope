import { debug } from "./debug.js";
import type { Fd } from "./fd.js";
import * as wasi from "./wasi_defs.js";

// TODO: there are several `ts-ignore` comments in this file that
// definitely need to go away. There are also some general improvements
// to be made later. But let's just get it working for now.

export interface Options {
  debug?: boolean;
}

/**
 * An exception that is thrown when the process exits.
 **/
export class WASIProcExit extends Error {
  constructor(public readonly code: number) {
    super(`exit with exit code ${code}`);
  }
}

export interface WASMInstance {
  // FIXME v0.3: close opened Fds after execution
  exports: { memory: WebAssembly.Memory; _start: () => unknown };
}

export default class WASI {
  args: Array<string> = [];
  env: Array<string> = [];
  fds: Array<Fd> = [];
  // SAFETY: definitely initialized before use.
  //@ts-ignore
  inst: { exports: { memory: WebAssembly.Memory } };
  wasiImport: { [key: string]: (...args: Array<any>) => unknown };

  /// Start a WASI command
  start(instance: WASMInstance) {
    this.inst = instance;
    try {
      instance.exports._start();
      return 0;
    } catch (e) {
      if (e instanceof WASIProcExit) {
        return e.code;
      } else {
        throw e;
      }
    }
  }

  /// Initialize a WASI reactor
  initialize(instance: {
    exports: { memory: WebAssembly.Memory; _initialize?: () => unknown };
  }) {
    this.inst = instance;
    if (instance.exports._initialize) {
      instance.exports._initialize();
    }
  }

  constructor(
    args: Array<string>,
    env: Array<string>,
    fds: Array<Fd>,
    options: Options = {}
  ) {
    debug.enable(options.debug);

    this.args = args;
    this.env = env;
    this.fds = fds;
    const self = this;
    this.wasiImport = {
      args_sizes_get(argc: number, argv_buf_size: number): number {
        const buffer = new DataView(self.inst.exports.memory.buffer);
        buffer.setUint32(argc, self.args.length, true);
        let buf_size = 0;
        for (const arg of self.args) {
          buf_size += arg.length + 1;
        }
        buffer.setUint32(argv_buf_size, buf_size, true);
        debug.log(
          buffer.getUint32(argc, true),
          buffer.getUint32(argv_buf_size, true)
        );
        return 0;
      },
      args_get(argv: number, argv_buf: number): number {
        const buffer = new DataView(self.inst.exports.memory.buffer);
        const buffer8 = new Uint8Array(self.inst.exports.memory.buffer);
        const orig_argv_buf = argv_buf;
        for (let i = 0; i < self.args.length; i++) {
          buffer.setUint32(argv, argv_buf, true);
          argv += 4;
          const arg = new TextEncoder().encode(self.args[i]);
          buffer8.set(arg, argv_buf);
          buffer.setUint8(argv_buf + arg.length, 0);
          argv_buf += arg.length + 1;
        }
        if (debug.enabled) {
          debug.log(
            new TextDecoder("utf-8").decode(
              buffer8.slice(orig_argv_buf, argv_buf)
            )
          );
        }
        return 0;
      },

      environ_sizes_get(environ_count: number, environ_size: number): number {
        const buffer = new DataView(self.inst.exports.memory.buffer);
        buffer.setUint32(environ_count, self.env.length, true);
        let buf_size = 0;
        for (const environ of self.env) {
          buf_size += environ.length + 1;
        }
        buffer.setUint32(environ_size, buf_size, true);
        debug.log(
          buffer.getUint32(environ_count, true),
          buffer.getUint32(environ_size, true)
        );
        return 0;
      },
      environ_get(environ: number, environ_buf: number): number {
        const buffer = new DataView(self.inst.exports.memory.buffer);
        const buffer8 = new Uint8Array(self.inst.exports.memory.buffer);
        const orig_environ_buf = environ_buf;
        for (let i = 0; i < self.env.length; i++) {
          buffer.setUint32(environ, environ_buf, true);
          environ += 4;
          const e = new TextEncoder().encode(self.env[i]);
          buffer8.set(e, environ_buf);
          buffer.setUint8(environ_buf + e.length, 0);
          environ_buf += e.length + 1;
        }
        if (debug.enabled) {
          debug.log(
            new TextDecoder("utf-8").decode(
              buffer8.slice(orig_environ_buf, environ_buf)
            )
          );
        }
        return 0;
      },

      // eslint-disable-next-line @typescript-eslint/no-unused-vars
      clock_res_get(id: number, res_ptr: number): number {
        let resolutionValue: bigint;
        switch (id) {
          case wasi.CLOCKID_MONOTONIC: {
            // https://developer.mozilla.org/en-US/docs/Web/API/Performance/now
            // > Resolution in isolated contexts: 5 microseconds
            resolutionValue = 5_000n; // 5 microseconds
            break;
          }
          case wasi.CLOCKID_REALTIME: {
            resolutionValue = 1_000_000n; // 1 millisecond?
            break;
          }
          default:
            return wasi.ERRNO_NOSYS;
        }
        const view = new DataView(self.inst.exports.memory.buffer);
        view.setBigUint64(res_ptr, resolutionValue, true);
        return wasi.ERRNO_SUCCESS;
      },
      clock_time_get(id: number, precision: bigint, time: number): number {
        const buffer = new DataView(self.inst.exports.memory.buffer);
        if (id === wasi.CLOCKID_REALTIME) {
          buffer.setBigUint64(
            time,
            BigInt(new Date().getTime()) * 1_000_000n,
            true
          );
        } else if (id === wasi.CLOCKID_MONOTONIC) {
          let monotonic_time: bigint;
          try {
            monotonic_time = BigInt(Math.round(performance.now() * 1000000));
          } catch (e) {
            // performance.now() is only available in browsers.
            // TODO use the perf_hooks builtin module for NodeJS
            monotonic_time = 0n;
          }
          buffer.setBigUint64(time, monotonic_time, true);
        } else {
          // TODO
          buffer.setBigUint64(time, 0n, true);
        }
        return 0;
      },

      fd_advise(
        fd: number,
        // eslint-disable-next-line @typescript-eslint/no-unused-vars
        offset: bigint,
        // eslint-disable-next-line @typescript-eslint/no-unused-vars
        len: bigint,
        // eslint-disable-next-line @typescript-eslint/no-unused-vars
        advice: number
      ): number {
        if (self.fds[fd] !== undefined) {
          return wasi.ERRNO_SUCCESS;
        } else {
          return wasi.ERRNO_BADF;
        }
      },
      fd_allocate(fd: number, offset: bigint, len: bigint): number {
        if (self.fds[fd] !== undefined) {
          return self.fds[fd].fd_allocate(offset, len);
        } else {
          return wasi.ERRNO_BADF;
        }
      },
      fd_close(fd: number): number {
        if (self.fds[fd] !== undefined) {
          const ret = self.fds[fd].fd_close();
          //@ts-ignore
          self.fds[fd] = undefined;
          return ret;
        } else {
          return wasi.ERRNO_BADF;
        }
      },
      fd_datasync(fd: number): number {
        if (self.fds[fd] !== undefined) {
          return self.fds[fd].fd_sync();
        } else {
          return wasi.ERRNO_BADF;
        }
      },
      fd_fdstat_get(fd: number, fdstat_ptr: number): number {
        if (self.fds[fd] !== undefined) {
          const { ret, fdstat } = self.fds[fd].fd_fdstat_get();
          if (fdstat !== null) {
            fdstat.write_bytes(
              new DataView(self.inst.exports.memory.buffer),
              fdstat_ptr
            );
          }
          return ret;
        } else {
          return wasi.ERRNO_BADF;
        }
      },
      fd_fdstat_set_flags(fd: number, flags: number): number {
        if (self.fds[fd] !== undefined) {
          return self.fds[fd].fd_fdstat_set_flags(flags);
        } else {
          return wasi.ERRNO_BADF;
        }
      },
      fd_fdstat_set_rights(
        fd: number,
        fs_rights_base: bigint,
        fs_rights_inheriting: bigint
      ): number {
        if (self.fds[fd] !== undefined) {
          return self.fds[fd].fd_fdstat_set_rights(
            fs_rights_base,
            fs_rights_inheriting
          );
        } else {
          return wasi.ERRNO_BADF;
        }
      },
      fd_filestat_get(fd: number, filestat_ptr: number): number {
        if (self.fds[fd] !== undefined) {
          const { ret, filestat } = self.fds[fd].fd_filestat_get();
          if (filestat !== null) {
            filestat.write_bytes(
              new DataView(self.inst.exports.memory.buffer),
              filestat_ptr
            );
          }
          return ret;
        } else {
          return wasi.ERRNO_BADF;
        }
      },
      fd_filestat_set_size(fd: number, size: bigint): number {
        if (self.fds[fd] !== undefined) {
          return self.fds[fd].fd_filestat_set_size(size);
        } else {
          return wasi.ERRNO_BADF;
        }
      },
      fd_filestat_set_times(
        fd: number,
        atim: bigint,
        mtim: bigint,
        fst_flags: number
      ): number {
        if (self.fds[fd] !== undefined) {
          return self.fds[fd].fd_filestat_set_times(atim, mtim, fst_flags);
        } else {
          return wasi.ERRNO_BADF;
        }
      },
      fd_pread(
        fd: number,
        iovs_ptr: number,
        iovs_len: number,
        offset: bigint,
        nread_ptr: number
      ): number {
        const buffer = new DataView(self.inst.exports.memory.buffer);
        const buffer8 = new Uint8Array(self.inst.exports.memory.buffer);
        if (self.fds[fd] !== undefined) {
          const iovecs = wasi.Iovec.read_bytes_array(
            buffer,
            iovs_ptr,
            iovs_len
          );
          let nread = 0;
          for (const iovec of iovecs) {
            const { ret, data } = self.fds[fd].fd_pread(iovec.buf_len, offset);
            if (ret !== wasi.ERRNO_SUCCESS) {
              buffer.setUint32(nread_ptr, nread, true);
              return ret;
            }
            buffer8.set(data, iovec.buf);
            nread += data.length;
            offset += BigInt(data.length);
            if (data.length !== iovec.buf_len) {
              break;
            }
          }
          buffer.setUint32(nread_ptr, nread, true);
          return wasi.ERRNO_SUCCESS;
        } else {
          return wasi.ERRNO_BADF;
        }
      },
      fd_prestat_get(fd: number, buf_ptr: number): number {
        const buffer = new DataView(self.inst.exports.memory.buffer);
        if (self.fds[fd] !== undefined) {
          const { ret, prestat } = self.fds[fd].fd_prestat_get();
          if (prestat !== null) {
            prestat.write_bytes(buffer, buf_ptr);
          }
          return ret;
        } else {
          return wasi.ERRNO_BADF;
        }
      },

      fd_prestat_dir_name(
        fd: number,
        path_ptr: number,
        // eslint-disable-next-line @typescript-eslint/no-unused-vars
        path_len: number
      ): number {
        // FIXME don't ignore path_len
        if (self.fds[fd] !== undefined) {
          const { ret, prestat } = self.fds[fd].fd_prestat_get();
          if (prestat == null) {
            return ret;
          }
          const prestat_dir_name = prestat.inner.pr_name;

          const buffer8 = new Uint8Array(self.inst.exports.memory.buffer);
          buffer8.set(prestat_dir_name.slice(0, path_len), path_ptr);

          return prestat_dir_name.byteLength > path_len
            ? wasi.ERRNO_NAMETOOLONG
            : wasi.ERRNO_SUCCESS;
        } else {
          return wasi.ERRNO_BADF;
        }
      },
      fd_pwrite(
        fd: number,
        iovs_ptr: number,
        iovs_len: number,
        offset: bigint,
        nwritten_ptr: number
      ): number {
        const buffer = new DataView(self.inst.exports.memory.buffer);
        const buffer8 = new Uint8Array(self.inst.exports.memory.buffer);
        if (self.fds[fd] !== undefined) {
          const iovecs = wasi.Ciovec.read_bytes_array(
            buffer,
            iovs_ptr,
            iovs_len
          );
          let nwritten = 0;
          for (const iovec of iovecs) {
            const data = buffer8.slice(iovec.buf, iovec.buf + iovec.buf_len);
            const { ret, nwritten: nwritten_part } = self.fds[fd].fd_pwrite(
              data,
              offset
            );
            if (ret !== wasi.ERRNO_SUCCESS) {
              buffer.setUint32(nwritten_ptr, nwritten, true);
              return ret;
            }
            nwritten += nwritten_part;
            offset += BigInt(nwritten_part);
            if (nwritten_part !== data.byteLength) {
              break;
            }
          }
          buffer.setUint32(nwritten_ptr, nwritten, true);
          return wasi.ERRNO_SUCCESS;
        } else {
          return wasi.ERRNO_BADF;
        }
      },
      fd_read(
        fd: number,
        iovs_ptr: number,
        iovs_len: number,
        nread_ptr: number
      ): number {
        const buffer = new DataView(self.inst.exports.memory.buffer);
        const buffer8 = new Uint8Array(self.inst.exports.memory.buffer);
        if (self.fds[fd] !== undefined) {
          const iovecs = wasi.Iovec.read_bytes_array(
            buffer,
            iovs_ptr,
            iovs_len
          );
          let nread = 0;
          for (const iovec of iovecs) {
            const { ret, data } = self.fds[fd].fd_read(iovec.buf_len);
            if (ret !== wasi.ERRNO_SUCCESS) {
              buffer.setUint32(nread_ptr, nread, true);
              return ret;
            }
            buffer8.set(data, iovec.buf);
            nread += data.length;
            if (data.length !== iovec.buf_len) {
              break;
            }
          }
          buffer.setUint32(nread_ptr, nread, true);
          return wasi.ERRNO_SUCCESS;
        } else {
          return wasi.ERRNO_BADF;
        }
      },
      fd_readdir(
        fd: number,
        buf: number,
        buf_len: number,
        cookie: bigint,
        bufused_ptr: number
      ): number {
        const buffer = new DataView(self.inst.exports.memory.buffer);
        const buffer8 = new Uint8Array(self.inst.exports.memory.buffer);
        if (self.fds[fd] !== undefined) {
          let bufused = 0;

          // eslint-disable-next-line no-constant-condition
          while (true) {
            const { ret, dirent } = self.fds[fd].fd_readdir_single(cookie);
            if (ret !== 0) {
              buffer.setUint32(bufused_ptr, bufused, true);
              return ret;
            }
            if (dirent == null) {
              break;
            }

            if (buf_len - bufused < dirent.head_length()) {
              bufused = buf_len;
              break;
            }

            const head_bytes = new ArrayBuffer(dirent.head_length());
            dirent.write_head_bytes(new DataView(head_bytes), 0);
            buffer8.set(
              new Uint8Array(head_bytes).slice(
                0,
                Math.min(head_bytes.byteLength, buf_len - bufused)
              ),
              buf
            );
            buf += dirent.head_length();
            bufused += dirent.head_length();

            if (buf_len - bufused < dirent.name_length()) {
              bufused = buf_len;
              break;
            }

            dirent.write_name_bytes(buffer8, buf, buf_len - bufused);
            buf += dirent.name_length();
            bufused += dirent.name_length();

            cookie = dirent.d_next;
          }

          buffer.setUint32(bufused_ptr, bufused, true);
          return 0;
        } else {
          return wasi.ERRNO_BADF;
        }
      },
      fd_renumber(fd: number, to: number) {
        if (self.fds[fd] !== undefined && self.fds[to] !== undefined) {
          const ret = self.fds[to].fd_close();
          if (ret !== 0) {
            return ret;
          }
          self.fds[to] = self.fds[fd];
          //@ts-ignore
          self.fds[fd] = undefined;
          return 0;
        } else {
          return wasi.ERRNO_BADF;
        }
      },
      fd_seek(
        fd: number,
        offset: bigint,
        whence: number,
        offset_out_ptr: number
      ): number {
        const buffer = new DataView(self.inst.exports.memory.buffer);
        if (self.fds[fd] !== undefined) {
          const { ret, offset: offset_out } = self.fds[fd].fd_seek(
            offset,
            whence
          );
          buffer.setBigInt64(offset_out_ptr, offset_out, true);
          return ret;
        } else {
          return wasi.ERRNO_BADF;
        }
      },
      fd_sync(fd: number): number {
        if (self.fds[fd] !== undefined) {
          return self.fds[fd].fd_sync();
        } else {
          return wasi.ERRNO_BADF;
        }
      },
      fd_tell(fd: number, offset_ptr: number): number {
        const buffer = new DataView(self.inst.exports.memory.buffer);
        if (self.fds[fd] !== undefined) {
          const { ret, offset } = self.fds[fd].fd_tell();
          buffer.setBigUint64(offset_ptr, offset, true);
          return ret;
        } else {
          return wasi.ERRNO_BADF;
        }
      },
      fd_write(
        fd: number,
        iovs_ptr: number,
        iovs_len: number,
        nwritten_ptr: number
      ): number {
        const buffer = new DataView(self.inst.exports.memory.buffer);
        const buffer8 = new Uint8Array(self.inst.exports.memory.buffer);
        if (self.fds[fd] !== undefined) {
          const iovecs = wasi.Ciovec.read_bytes_array(
            buffer,
            iovs_ptr,
            iovs_len
          );
          let nwritten = 0;
          for (const iovec of iovecs) {
            const data = buffer8.slice(iovec.buf, iovec.buf + iovec.buf_len);
            const { ret, nwritten: nwritten_part } =
              self.fds[fd].fd_write(data);
            if (ret !== wasi.ERRNO_SUCCESS) {
              buffer.setUint32(nwritten_ptr, nwritten, true);
              return ret;
            }
            nwritten += nwritten_part;
            if (nwritten_part !== data.byteLength) {
              break;
            }
          }
          buffer.setUint32(nwritten_ptr, nwritten, true);
          return wasi.ERRNO_SUCCESS;
        } else {
          return wasi.ERRNO_BADF;
        }
      },
      path_create_directory(
        fd: number,
        path_ptr: number,
        path_len: number
      ): number {
        const buffer8 = new Uint8Array(self.inst.exports.memory.buffer);
        if (self.fds[fd] !== undefined) {
          const path = new TextDecoder("utf-8").decode(
            buffer8.slice(path_ptr, path_ptr + path_len)
          );
          return self.fds[fd].path_create_directory(path);
        } else {
          return wasi.ERRNO_BADF;
        }
      },
      path_filestat_get(
        fd: number,
        flags: number,
        path_ptr: number,
        path_len: number,
        filestat_ptr: number
      ): number {
        const buffer = new DataView(self.inst.exports.memory.buffer);
        const buffer8 = new Uint8Array(self.inst.exports.memory.buffer);
        if (self.fds[fd] !== undefined) {
          const path = new TextDecoder("utf-8").decode(
            buffer8.slice(path_ptr, path_ptr + path_len)
          );
          const { ret, filestat } = self.fds[fd].path_filestat_get(flags, path);
          if (filestat != null) {
            filestat.write_bytes(buffer, filestat_ptr);
          }
          return ret;
        } else {
          return wasi.ERRNO_BADF;
        }
      },
      path_filestat_set_times(
        fd: number,
        flags: number,
        path_ptr: number,
        path_len: number,
        atim: bigint,
        mtim: bigint,
        fst_flags: number
      ) {
        const buffer8 = new Uint8Array(self.inst.exports.memory.buffer);
        if (self.fds[fd] !== undefined) {
          const path = new TextDecoder("utf-8").decode(
            buffer8.slice(path_ptr, path_ptr + path_len)
          );
          return self.fds[fd].path_filestat_set_times(
            flags,
            path,
            atim,
            mtim,
            fst_flags
          );
        } else {
          return wasi.ERRNO_BADF;
        }
      },
      path_link(
        old_fd: number,
        old_flags,
        old_path_ptr: number,
        old_path_len: number,
        new_fd: number,
        new_path_ptr: number,
        new_path_len: number
      ): number {
        const buffer8 = new Uint8Array(self.inst.exports.memory.buffer);
        if (self.fds[old_fd] !== undefined && self.fds[new_fd] !== undefined) {
          const old_path = new TextDecoder("utf-8").decode(
            buffer8.slice(old_path_ptr, old_path_ptr + old_path_len)
          );
          const new_path = new TextDecoder("utf-8").decode(
            buffer8.slice(new_path_ptr, new_path_ptr + new_path_len)
          );
          const { ret, inode_obj } = self.fds[old_fd].path_lookup(
            old_path,
            old_flags
          );
          if (inode_obj == null) {
            return ret;
          }
          return self.fds[new_fd].path_link(new_path, inode_obj, false);
        } else {
          return wasi.ERRNO_BADF;
        }
      },
      path_open(
        fd: number,
        dirflags,
        path_ptr: number,
        path_len: number,
        oflags,
        fs_rights_base,
        fs_rights_inheriting,
        fd_flags,
        opened_fd_ptr: number
      ): number {
        const buffer = new DataView(self.inst.exports.memory.buffer);
        const buffer8 = new Uint8Array(self.inst.exports.memory.buffer);
        if (self.fds[fd] !== undefined) {
          const path = new TextDecoder("utf-8").decode(
            buffer8.slice(path_ptr, path_ptr + path_len)
          );
          debug.log(path);
          const { ret, fd_obj } = self.fds[fd].path_open(
            dirflags,
            path,
            oflags,
            fs_rights_base,
            fs_rights_inheriting,
            fd_flags
          );
          if (ret !== 0) {
            return ret;
          }
          // FIXME use first free fd
          //@ts-ignore
          self.fds.push(fd_obj);
          const opened_fd = self.fds.length - 1;
          buffer.setUint32(opened_fd_ptr, opened_fd, true);
          return 0;
        } else {
          return wasi.ERRNO_BADF;
        }
      },
      path_readlink(
        fd: number,
        path_ptr: number,
        path_len: number,
        buf_ptr: number,
        buf_len: number,
        nread_ptr: number
      ): number {
        const buffer = new DataView(self.inst.exports.memory.buffer);
        const buffer8 = new Uint8Array(self.inst.exports.memory.buffer);
        if (self.fds[fd] !== undefined) {
          const path = new TextDecoder("utf-8").decode(
            buffer8.slice(path_ptr, path_ptr + path_len)
          );
          debug.log(path);
          const { ret, data } = self.fds[fd].path_readlink(path);
          if (data !== null) {
            const data_buf = new TextEncoder().encode(data);
            if (data_buf.length > buf_len) {
              buffer.setUint32(nread_ptr, 0, true);
              return wasi.ERRNO_BADF;
            }
            buffer8.set(data_buf, buf_ptr);
            buffer.setUint32(nread_ptr, data_buf.length, true);
          }
          return ret;
        } else {
          return wasi.ERRNO_BADF;
        }
      },
      path_remove_directory(
        fd: number,
        path_ptr: number,
        path_len: number
      ): number {
        const buffer8 = new Uint8Array(self.inst.exports.memory.buffer);
        if (self.fds[fd] !== undefined) {
          const path = new TextDecoder("utf-8").decode(
            buffer8.slice(path_ptr, path_ptr + path_len)
          );
          return self.fds[fd].path_remove_directory(path);
        } else {
          return wasi.ERRNO_BADF;
        }
      },
      path_rename(
        fd: number,
        old_path_ptr: number,
        old_path_len: number,
        new_fd: number,
        new_path_ptr: number,
        new_path_len: number
      ): number {
        const buffer8 = new Uint8Array(self.inst.exports.memory.buffer);
        if (self.fds[fd] !== undefined && self.fds[new_fd] !== undefined) {
          const old_path = new TextDecoder("utf-8").decode(
            buffer8.slice(old_path_ptr, old_path_ptr + old_path_len)
          );
          const new_path = new TextDecoder("utf-8").decode(
            buffer8.slice(new_path_ptr, new_path_ptr + new_path_len)
          );
          // eslint-disable-next-line prefer-const
          let { ret, inode_obj } = self.fds[fd].path_unlink(old_path);
          if (inode_obj == null) {
            return ret;
          }
          ret = self.fds[new_fd].path_link(new_path, inode_obj, true);
          if (ret !== wasi.ERRNO_SUCCESS) {
            if (
              self.fds[fd].path_link(old_path, inode_obj, true) !==
              wasi.ERRNO_SUCCESS
            ) {
              throw "path_link should always return success when relinking an inode back to the original place";
            }
          }
          return ret;
        } else {
          return wasi.ERRNO_BADF;
        }
      },
      path_symlink(
        old_path_ptr: number,
        old_path_len: number,
        fd: number,
        new_path_ptr: number,
        new_path_len: number
      ): number {
        const buffer8 = new Uint8Array(self.inst.exports.memory.buffer);
        if (self.fds[fd] !== undefined) {
          // eslint-disable-next-line @typescript-eslint/no-unused-vars
          const old_path = new TextDecoder("utf-8").decode(
            buffer8.slice(old_path_ptr, old_path_ptr + old_path_len)
          );
          // eslint-disable-next-line @typescript-eslint/no-unused-vars
          const new_path = new TextDecoder("utf-8").decode(
            buffer8.slice(new_path_ptr, new_path_ptr + new_path_len)
          );
          return wasi.ERRNO_NOTSUP;
        } else {
          return wasi.ERRNO_BADF;
        }
      },
      path_unlink_file(fd: number, path_ptr: number, path_len: number): number {
        const buffer8 = new Uint8Array(self.inst.exports.memory.buffer);
        if (self.fds[fd] !== undefined) {
          const path = new TextDecoder("utf-8").decode(
            buffer8.slice(path_ptr, path_ptr + path_len)
          );
          return self.fds[fd].path_unlink_file(path);
        } else {
          return wasi.ERRNO_BADF;
        }
      },
      // eslint-disable-next-line @typescript-eslint/no-unused-vars
      poll_oneoff(in_, out, nsubscriptions) {
        throw "async io not supported";
      },
      proc_exit(exit_code: number) {
        throw new WASIProcExit(exit_code);
      },
      proc_raise(sig: number) {
        throw `raised signal ${sig}`;
      },
      sched_yield() {},
      random_get(buf: number, buf_len: number) {
        const buffer8 = new Uint8Array(
          self.inst.exports.memory.buffer
        ).subarray(buf, buf + buf_len);

        if (
          "crypto" in globalThis &&
          !(self.inst.exports.memory.buffer instanceof SharedArrayBuffer)
        ) {
          for (let i = 0; i < buf_len; i += 65536) {
            crypto.getRandomValues(buffer8.subarray(i, i + 65536));
          }
        } else {
          for (let i = 0; i < buf_len; i++) {
            buffer8[i] = (Math.random() * 256) | 0;
          }
        }
      },
      // eslint-disable-next-line @typescript-eslint/no-unused-vars
      sock_recv(fd: number, ri_data, ri_flags) {
        throw "sockets not supported";
      },
      // eslint-disable-next-line @typescript-eslint/no-unused-vars
      sock_send(fd: number, si_data, si_flags) {
        throw "sockets not supported";
      },
      // eslint-disable-next-line @typescript-eslint/no-unused-vars
      sock_shutdown(fd: number, how) {
        throw "sockets not supported";
      },
      // eslint-disable-next-line @typescript-eslint/no-unused-vars
      sock_accept(fd: number, flags) {
        throw "sockets not supported";
      }
    };
  }
}
