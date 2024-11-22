import { debug } from "./debug.js";
import { Fd, Inode } from "./fd.js";
import * as wasi from "./wasi_defs.js";

export class OpenFile extends Fd {
  file: File;
  file_pos = 0n;

  constructor(file: File) {
    super();
    this.file = file;
  }

  fd_allocate(offset: bigint, len: bigint): number {
    if (this.file.size > offset + len) {
      // already big enough
    } else {
      // extend
      const new_data = new Uint8Array(Number(offset + len));
      new_data.set(this.file.data, 0);
      this.file.data = new_data;
    }
    return wasi.ERRNO_SUCCESS;
  }

  fd_fdstat_get(): { ret: number; fdstat: wasi.Fdstat | null } {
    return { ret: 0, fdstat: new wasi.Fdstat(wasi.FILETYPE_REGULAR_FILE, 0) };
  }

  fd_filestat_set_size(size: bigint): number {
    if (this.file.size > size) {
      // truncate
      this.file.data = new Uint8Array(
        this.file.data.buffer.slice(0, Number(size))
      );
    } else {
      // extend
      const new_data = new Uint8Array(Number(size));
      new_data.set(this.file.data, 0);
      this.file.data = new_data;
    }
    return wasi.ERRNO_SUCCESS;
  }

  fd_read(size: number): { ret: number; data: Uint8Array } {
    const slice = this.file.data.slice(
      Number(this.file_pos),
      Number(this.file_pos + BigInt(size))
    );
    this.file_pos += BigInt(slice.length);
    return { ret: 0, data: slice };
  }

  fd_pread(size: number, offset: bigint): { ret: number; data: Uint8Array } {
    const slice = this.file.data.slice(
      Number(offset),
      Number(offset + BigInt(size))
    );
    return { ret: 0, data: slice };
  }

  fd_seek(offset: bigint, whence: number): { ret: number; offset: bigint } {
    let calculated_offset: bigint;
    switch (whence) {
      case wasi.WHENCE_SET:
        calculated_offset = offset;
        break;
      case wasi.WHENCE_CUR:
        calculated_offset = this.file_pos + offset;
        break;
      case wasi.WHENCE_END:
        calculated_offset = BigInt(this.file.data.byteLength) + offset;
        break;
      default:
        return { ret: wasi.ERRNO_INVAL, offset: 0n };
    }

    if (calculated_offset < 0) {
      return { ret: wasi.ERRNO_INVAL, offset: 0n };
    }

    this.file_pos = calculated_offset;
    return { ret: 0, offset: this.file_pos };
  }

  fd_tell(): { ret: number; offset: bigint } {
    return { ret: 0, offset: this.file_pos };
  }

  fd_write(data: Uint8Array): { ret: number; nwritten: number } {
    if (this.file.readonly) return { ret: wasi.ERRNO_BADF, nwritten: 0 };

    if (this.file_pos + BigInt(data.byteLength) > this.file.size) {
      const old = this.file.data;
      this.file.data = new Uint8Array(
        Number(this.file_pos + BigInt(data.byteLength))
      );
      this.file.data.set(old);
    }

    this.file.data.set(data, Number(this.file_pos));
    this.file_pos += BigInt(data.byteLength);
    return { ret: 0, nwritten: data.byteLength };
  }

  fd_pwrite(data: Uint8Array, offset: bigint) {
    if (this.file.readonly) return { ret: wasi.ERRNO_BADF, nwritten: 0 };

    if (offset + BigInt(data.byteLength) > this.file.size) {
      const old = this.file.data;
      this.file.data = new Uint8Array(Number(offset + BigInt(data.byteLength)));
      this.file.data.set(old);
    }

    this.file.data.set(data, Number(offset));
    return { ret: 0, nwritten: data.byteLength };
  }

  fd_filestat_get(): { ret: number; filestat: wasi.Filestat } {
    return { ret: 0, filestat: this.file.stat() };
  }
}

export class OpenDirectory extends Fd {
  dir: Directory;

  constructor(dir: Directory) {
    super();
    this.dir = dir;
  }

  // eslint-disable-next-line @typescript-eslint/no-unused-vars
  fd_seek(offset: bigint, whence: number): { ret: number; offset: bigint } {
    return { ret: wasi.ERRNO_BADF, offset: 0n };
  }

  fd_tell(): { ret: number; offset: bigint } {
    return { ret: wasi.ERRNO_BADF, offset: 0n };
  }

  // eslint-disable-next-line @typescript-eslint/no-unused-vars
  fd_allocate(offset: bigint, len: bigint): number {
    return wasi.ERRNO_BADF;
  }

  fd_fdstat_get(): { ret: number; fdstat: wasi.Fdstat | null } {
    return { ret: 0, fdstat: new wasi.Fdstat(wasi.FILETYPE_DIRECTORY, 0) };
  }

  fd_readdir_single(cookie: bigint): {
    ret: number;
    dirent: wasi.Dirent | null;
  } {
    if (debug.enabled) {
      debug.log("readdir_single", cookie);
      debug.log(cookie, this.dir.contents.keys());
    }

    if (cookie === 0n) {
      return {
        ret: wasi.ERRNO_SUCCESS,
        dirent: new wasi.Dirent(1n, ".", wasi.FILETYPE_DIRECTORY)
      };
    } else if (cookie === 1n) {
      return {
        ret: wasi.ERRNO_SUCCESS,
        dirent: new wasi.Dirent(2n, "..", wasi.FILETYPE_DIRECTORY)
      };
    }

    if (cookie >= BigInt(this.dir.contents.size) + 2n) {
      return { ret: 0, dirent: null };
    }

    const [name, entry] = Array.from(this.dir.contents.entries())[
      Number(cookie - 2n)
    ];

    return {
      ret: 0,
      dirent: new wasi.Dirent(cookie + 1n, name, entry.stat().filetype)
    };
  }

  path_filestat_get(
    flags: number,
    path_str: string
  ): { ret: number; filestat: wasi.Filestat | null } {
    const { ret: path_err, path } = Path.from(path_str);
    if (path == null) {
      return { ret: path_err, filestat: null };
    }

    const { ret, entry } = this.dir.get_entry_for_path(path);
    if (entry == null) {
      return { ret, filestat: null };
    }

    return { ret: 0, filestat: entry.stat() };
  }

  path_lookup(
    path_str: string,
    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    dirflags: number
  ): { ret: number; inode_obj: Inode | null } {
    const { ret: path_ret, path } = Path.from(path_str);
    if (path == null) {
      return { ret: path_ret, inode_obj: null };
    }

    const { ret, entry } = this.dir.get_entry_for_path(path);
    if (entry == null) {
      return { ret, inode_obj: null };
    }

    return { ret: wasi.ERRNO_SUCCESS, inode_obj: entry };
  }

  path_open(
    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    dirflags: number,
    path_str: string,
    oflags: number,
    fs_rights_base: bigint,
    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    fs_rights_inheriting: bigint,
    fd_flags: number
  ): { ret: number; fd_obj: Fd | null } {
    const { ret: path_ret, path } = Path.from(path_str);
    if (path == null) {
      return { ret: path_ret, fd_obj: null };
    }

    // eslint-disable-next-line prefer-const
    let { ret, entry } = this.dir.get_entry_for_path(path);
    if (entry == null) {
      if (ret !== wasi.ERRNO_NOENT) {
        return { ret, fd_obj: null };
      }
      if ((oflags & wasi.OFLAGS_CREAT) === wasi.OFLAGS_CREAT) {
        // doesn't exist, but shall be created
        const { ret, entry: new_entry } = this.dir.create_entry_for_path(
          path_str,
          (oflags & wasi.OFLAGS_DIRECTORY) === wasi.OFLAGS_DIRECTORY
        );
        if (new_entry == null) {
          return { ret, fd_obj: null };
        }
        entry = new_entry;
      } else {
        // doesn't exist, no such file
        return { ret: wasi.ERRNO_NOENT, fd_obj: null };
      }
    } else if ((oflags & wasi.OFLAGS_EXCL) === wasi.OFLAGS_EXCL) {
      // was supposed to be created exclusively, but exists already
      return { ret: wasi.ERRNO_EXIST, fd_obj: null };
    }
    if (
      (oflags & wasi.OFLAGS_DIRECTORY) === wasi.OFLAGS_DIRECTORY &&
      entry.stat().filetype !== wasi.FILETYPE_DIRECTORY
    ) {
      // expected a directory but the file is not a directory
      return { ret: wasi.ERRNO_NOTDIR, fd_obj: null };
    }
    return entry.path_open(oflags, fs_rights_base, fd_flags);
  }

  path_create_directory(path: string): number {
    return this.path_open(
      0,
      path,
      wasi.OFLAGS_CREAT | wasi.OFLAGS_DIRECTORY,
      0n,
      0n,
      0
    ).ret;
  }

  path_link(path_str: string, inode: Inode, allow_dir: boolean): number {
    const { ret: path_ret, path } = Path.from(path_str);
    if (path == null) {
      return path_ret;
    }

    if (path.is_dir) {
      return wasi.ERRNO_NOENT;
    }

    const {
      ret: parent_ret,
      parent_entry,
      filename,
      entry
    } = this.dir.get_parent_dir_and_entry_for_path(path, true);
    if (parent_entry == null || filename == null) {
      return parent_ret;
    }

    if (entry != null) {
      const source_is_dir = inode.stat().filetype === wasi.FILETYPE_DIRECTORY;
      const target_is_dir = entry.stat().filetype === wasi.FILETYPE_DIRECTORY;
      if (source_is_dir && target_is_dir) {
        if (allow_dir && entry instanceof Directory) {
          if (entry.contents.size === 0) {
            // Allow overwriting empty directories
          } else {
            return wasi.ERRNO_NOTEMPTY;
          }
        } else {
          return wasi.ERRNO_EXIST;
        }
      } else if (source_is_dir && !target_is_dir) {
        return wasi.ERRNO_NOTDIR;
      } else if (!source_is_dir && target_is_dir) {
        return wasi.ERRNO_ISDIR;
      } else if (
        inode.stat().filetype === wasi.FILETYPE_REGULAR_FILE &&
        entry.stat().filetype === wasi.FILETYPE_REGULAR_FILE
      ) {
        // Overwriting regular files is fine
      } else {
        return wasi.ERRNO_EXIST;
      }
    }

    if (!allow_dir && inode.stat().filetype === wasi.FILETYPE_DIRECTORY) {
      return wasi.ERRNO_PERM;
    }

    parent_entry.contents.set(filename, inode);

    return wasi.ERRNO_SUCCESS;
  }

  path_unlink(path_str: string): { ret: number; inode_obj: Inode | null } {
    const { ret: path_ret, path } = Path.from(path_str);
    if (path == null) {
      return { ret: path_ret, inode_obj: null };
    }

    const {
      ret: parent_ret,
      parent_entry,
      filename,
      entry
    } = this.dir.get_parent_dir_and_entry_for_path(path, true);
    if (parent_entry == null || filename == null) {
      return { ret: parent_ret, inode_obj: null };
    }

    if (entry == null) {
      return { ret: wasi.ERRNO_NOENT, inode_obj: null };
    }

    parent_entry.contents.delete(filename);

    return { ret: wasi.ERRNO_SUCCESS, inode_obj: entry };
  }

  path_unlink_file(path_str: string): number {
    const { ret: path_ret, path } = Path.from(path_str);
    if (path == null) {
      return path_ret;
    }

    const {
      ret: parent_ret,
      parent_entry,
      filename,
      entry
    } = this.dir.get_parent_dir_and_entry_for_path(path, false);
    if (parent_entry == null || filename == null || entry == null) {
      return parent_ret;
    }
    if (entry.stat().filetype === wasi.FILETYPE_DIRECTORY) {
      return wasi.ERRNO_ISDIR;
    }
    parent_entry.contents.delete(filename);
    return wasi.ERRNO_SUCCESS;
  }

  path_remove_directory(path_str: string): number {
    const { ret: path_ret, path } = Path.from(path_str);
    if (path == null) {
      return path_ret;
    }

    const {
      ret: parent_ret,
      parent_entry,
      filename,
      entry
    } = this.dir.get_parent_dir_and_entry_for_path(path, false);
    if (parent_entry == null || filename == null || entry == null) {
      return parent_ret;
    }

    if (
      !(entry instanceof Directory) ||
      entry.stat().filetype !== wasi.FILETYPE_DIRECTORY
    ) {
      return wasi.ERRNO_NOTDIR;
    }
    if (entry.contents.size !== 0) {
      return wasi.ERRNO_NOTEMPTY;
    }
    if (!parent_entry.contents.delete(filename)) {
      return wasi.ERRNO_NOENT;
    }
    return wasi.ERRNO_SUCCESS;
  }

  fd_filestat_get(): { ret: number; filestat: wasi.Filestat } {
    return { ret: 0, filestat: this.dir.stat() };
  }

  // eslint-disable-next-line @typescript-eslint/no-unused-vars
  fd_filestat_set_size(size: bigint): number {
    return wasi.ERRNO_BADF;
  }

  // eslint-disable-next-line @typescript-eslint/no-unused-vars
  fd_read(size: number): { ret: number; data: Uint8Array } {
    return { ret: wasi.ERRNO_BADF, data: new Uint8Array() };
  }

  // eslint-disable-next-line @typescript-eslint/no-unused-vars
  fd_pread(size: number, offset: bigint): { ret: number; data: Uint8Array } {
    return { ret: wasi.ERRNO_BADF, data: new Uint8Array() };
  }

  // eslint-disable-next-line @typescript-eslint/no-unused-vars
  fd_write(data: Uint8Array): { ret: number; nwritten: number } {
    return { ret: wasi.ERRNO_BADF, nwritten: 0 };
  }

  fd_pwrite(
    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    data: Uint8Array,
    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    offset: bigint
  ): { ret: number; nwritten: number } {
    return { ret: wasi.ERRNO_BADF, nwritten: 0 };
  }
}

export class PreopenDirectory extends OpenDirectory {
  prestat_name: string;

  constructor(name: string, contents: Map<string, Inode>) {
    super(new Directory(contents));
    this.prestat_name = name;
  }

  fd_prestat_get(): { ret: number; prestat: wasi.Prestat | null } {
    return {
      ret: 0,
      prestat: wasi.Prestat.dir(this.prestat_name)
    };
  }
}

export class File extends Inode {
  data: Uint8Array;
  readonly: boolean;

  constructor(
    data: ArrayBuffer | SharedArrayBuffer | Uint8Array | Array<number>,
    options?: Partial<{
      readonly: boolean;
    }>
  ) {
    super();
    this.data = new Uint8Array(data);
    this.readonly = !!options?.readonly;
  }

  path_open(oflags: number, fs_rights_base: bigint, fd_flags: number) {
    if (
      this.readonly &&
      (fs_rights_base & BigInt(wasi.RIGHTS_FD_WRITE)) ===
        BigInt(wasi.RIGHTS_FD_WRITE)
    ) {
      // no write permission to file
      return { ret: wasi.ERRNO_PERM, fd_obj: null };
    }

    if ((oflags & wasi.OFLAGS_TRUNC) === wasi.OFLAGS_TRUNC) {
      if (this.readonly) return { ret: wasi.ERRNO_PERM, fd_obj: null };
      this.data = new Uint8Array([]);
    }

    const file = new OpenFile(this);
    if (fd_flags & wasi.FDFLAGS_APPEND) file.fd_seek(0n, wasi.WHENCE_END);
    return { ret: wasi.ERRNO_SUCCESS, fd_obj: file };
  }

  get size(): bigint {
    return BigInt(this.data.byteLength);
  }

  stat(): wasi.Filestat {
    return new wasi.Filestat(wasi.FILETYPE_REGULAR_FILE, this.size);
  }
}

class Path {
  parts: string[] = [];
  is_dir = false;

  static from(path: string): { ret: number; path: Path | null } {
    const self = new Path();
    self.is_dir = path.endsWith("/");

    if (path.startsWith("/")) {
      return { ret: wasi.ERRNO_NOTCAPABLE, path: null };
    }
    if (path.includes("\0")) {
      return { ret: wasi.ERRNO_INVAL, path: null };
    }

    for (const component of path.split("/")) {
      if (component === "" || component === ".") {
        continue;
      }
      if (component === "..") {
        if (self.parts.pop() === undefined) {
          return { ret: wasi.ERRNO_NOTCAPABLE, path: null };
        }
        continue;
      }
      self.parts.push(component);
    }

    return { ret: wasi.ERRNO_SUCCESS, path: self };
  }

  to_path_string(): string {
    let s = this.parts.join("/");
    if (this.is_dir) {
      s += "/";
    }
    return s;
  }
}

export class Directory extends Inode {
  contents: Map<string, Inode>;

  constructor(contents: Map<string, Inode> | [string, Inode][]) {
    super();
    if (Array.isArray(contents)) {
      this.contents = new Map(contents);
    } else {
      this.contents = contents;
    }
  }

  // eslint-disable-next-line @typescript-eslint/no-unused-vars
  path_open(oflags: number, fs_rights_base: bigint, fd_flags: number) {
    return { ret: wasi.ERRNO_SUCCESS, fd_obj: new OpenDirectory(this) };
  }

  stat(): wasi.Filestat {
    return new wasi.Filestat(wasi.FILETYPE_DIRECTORY, 0n);
  }

  get_entry_for_path(path: Path): { ret: number; entry: Inode | null } {
    let entry: Inode = this;
    for (const component of path.parts) {
      if (!(entry instanceof Directory)) {
        return { ret: wasi.ERRNO_NOTDIR, entry: null };
      }
      const child = entry.contents.get(component);
      if (child !== undefined) {
        entry = child;
      } else {
        debug.log(component);
        return { ret: wasi.ERRNO_NOENT, entry: null };
      }
    }

    if (path.is_dir) {
      if (entry.stat().filetype !== wasi.FILETYPE_DIRECTORY) {
        return { ret: wasi.ERRNO_NOTDIR, entry: null };
      }
    }

    return { ret: wasi.ERRNO_SUCCESS, entry };
  }

  get_parent_dir_and_entry_for_path(
    path: Path,
    allow_undefined: boolean
  ): {
    ret: number;
    parent_entry: Directory | null;
    filename: string | null;
    entry: Inode | null;
  } {
    const filename = path.parts.pop();

    if (filename === undefined) {
      return {
        ret: wasi.ERRNO_INVAL,
        parent_entry: null,
        filename: null,
        entry: null
      };
    }

    const { ret: entry_ret, entry: parent_entry } =
      this.get_entry_for_path(path);
    if (parent_entry == null) {
      return {
        ret: entry_ret,
        parent_entry: null,
        filename: null,
        entry: null
      };
    }
    if (!(parent_entry instanceof Directory)) {
      return {
        ret: wasi.ERRNO_NOTDIR,
        parent_entry: null,
        filename: null,
        entry: null
      };
    }
    const entry: Inode | undefined | null = parent_entry.contents.get(filename);
    if (entry === undefined) {
      if (!allow_undefined) {
        return {
          ret: wasi.ERRNO_NOENT,
          parent_entry: null,
          filename: null,
          entry: null
        };
      } else {
        return { ret: wasi.ERRNO_SUCCESS, parent_entry, filename, entry: null };
      }
    }

    if (path.is_dir) {
      if (entry.stat().filetype !== wasi.FILETYPE_DIRECTORY) {
        return {
          ret: wasi.ERRNO_NOTDIR,
          parent_entry: null,
          filename: null,
          entry: null
        };
      }
    }

    return { ret: wasi.ERRNO_SUCCESS, parent_entry, filename, entry };
  }

  create_entry_for_path(
    path_str: string,
    is_dir: boolean
  ): { ret: number; entry: Inode | null } {
    const { ret: path_ret, path } = Path.from(path_str);
    if (path == null) {
      return { ret: path_ret, entry: null };
    }

    let {
      // eslint-disable-next-line prefer-const
      ret: parent_ret,
      // eslint-disable-next-line prefer-const
      parent_entry,
      // eslint-disable-next-line prefer-const
      filename,
      entry
    } = this.get_parent_dir_and_entry_for_path(path, true);
    if (parent_entry == null || filename == null) {
      return { ret: parent_ret, entry: null };
    }

    if (entry != null) {
      return { ret: wasi.ERRNO_EXIST, entry: null };
    }

    debug.log("create", path);
    let new_child;
    if (!is_dir) {
      new_child = new File(new ArrayBuffer(0));
    } else {
      new_child = new Directory(new Map());
    }
    parent_entry.contents.set(filename, new_child);
    entry = new_child;

    return { ret: wasi.ERRNO_SUCCESS, entry };
  }

  get_file(file: string): File | null {
    const f = this.contents.get(file);
    if (f instanceof File) {
      return f;
    } else {
      return null;
    }
  }
}

export class ConsoleStdout extends Fd {
  write: (buffer: Uint8Array) => void;

  constructor(write: (buffer: Uint8Array) => void) {
    super();
    this.write = write;
  }

  fd_filestat_get(): { ret: number; filestat: wasi.Filestat } {
    const filestat = new wasi.Filestat(
      wasi.FILETYPE_CHARACTER_DEVICE,
      BigInt(0)
    );
    return { ret: 0, filestat };
  }

  fd_fdstat_get(): { ret: number; fdstat: wasi.Fdstat | null } {
    const fdstat = new wasi.Fdstat(wasi.FILETYPE_CHARACTER_DEVICE, 0);
    fdstat.fs_rights_base = BigInt(wasi.RIGHTS_FD_WRITE);
    return { ret: 0, fdstat };
  }

  fd_write(data: Uint8Array): { ret: number; nwritten: number } {
    this.write(data);
    return { ret: 0, nwritten: data.byteLength };
  }

  static lineBuffered(write: (line: string) => void): ConsoleStdout {
    const dec = new TextDecoder("utf-8", { fatal: false });
    let line_buf = "";
    return new ConsoleStdout(buffer => {
      line_buf += dec.decode(buffer, { stream: true });
      const lines = line_buf.split("\n");
      for (const [i, line] of lines.entries()) {
        if (i < lines.length - 1) {
          write(line);
        } else {
          line_buf = line;
        }
      }
    });
  }
}
