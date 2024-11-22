// Based on https://github.com/bytecodealliance/wasi/tree/d3c7a34193cb33d994b11104b22d234530232b5f

export const FD_STDIN = 0;
export const FD_STDOUT = 1;
export const FD_STDERR = 2;

export const CLOCKID_REALTIME = 0;
export const CLOCKID_MONOTONIC = 1;
export const CLOCKID_PROCESS_CPUTIME_ID = 2;
export const CLOCKID_THREAD_CPUTIME_ID = 3;

export const ERRNO_SUCCESS = 0;
export const ERRNO_2BIG = 1;
export const ERRNO_ACCES = 2;
export const ERRNO_ADDRINUSE = 3;
export const ERRNO_ADDRNOTAVAIL = 4;
export const ERRNO_AFNOSUPPORT = 5;
export const ERRNO_AGAIN = 6;
export const ERRNO_ALREADY = 7;
export const ERRNO_BADF = 8;
export const ERRNO_BADMSG = 9;
export const ERRNO_BUSY = 10;
export const ERRNO_CANCELED = 11;
export const ERRNO_CHILD = 12;
export const ERRNO_CONNABORTED = 13;
export const ERRNO_CONNREFUSED = 14;
export const ERRNO_CONNRESET = 15;
export const ERRNO_DEADLK = 16;
export const ERRNO_DESTADDRREQ = 17;
export const ERRNO_DOM = 18;
export const ERRNO_DQUOT = 19;
export const ERRNO_EXIST = 20;
export const ERRNO_FAULT = 21;
export const ERRNO_FBIG = 22;
export const ERRNO_HOSTUNREACH = 23;
export const ERRNO_IDRM = 24;
export const ERRNO_ILSEQ = 25;
export const ERRNO_INPROGRESS = 26;
export const ERRNO_INTR = 27;
export const ERRNO_INVAL = 28;
export const ERRNO_IO = 29;
export const ERRNO_ISCONN = 30;
export const ERRNO_ISDIR = 31;
export const ERRNO_LOOP = 32;
export const ERRNO_MFILE = 33;
export const ERRNO_MLINK = 34;
export const ERRNO_MSGSIZE = 35;
export const ERRNO_MULTIHOP = 36;
export const ERRNO_NAMETOOLONG = 37;
export const ERRNO_NETDOWN = 38;
export const ERRNO_NETRESET = 39;
export const ERRNO_NETUNREACH = 40;
export const ERRNO_NFILE = 41;
export const ERRNO_NOBUFS = 42;
export const ERRNO_NODEV = 43;
export const ERRNO_NOENT = 44;
export const ERRNO_NOEXEC = 45;
export const ERRNO_NOLCK = 46;
export const ERRNO_NOLINK = 47;
export const ERRNO_NOMEM = 48;
export const ERRNO_NOMSG = 49;
export const ERRNO_NOPROTOOPT = 50;
export const ERRNO_NOSPC = 51;
export const ERRNO_NOSYS = 52;
export const ERRNO_NOTCONN = 53;
export const ERRNO_NOTDIR = 54;
export const ERRNO_NOTEMPTY = 55;
export const ERRNO_NOTRECOVERABLE = 56;
export const ERRNO_NOTSOCK = 57;
export const ERRNO_NOTSUP = 58;
export const ERRNO_NOTTY = 59;
export const ERRNO_NXIO = 60;
export const ERRNO_OVERFLOW = 61;
export const ERRNO_OWNERDEAD = 62;
export const ERRNO_PERM = 63;
export const ERRNO_PIPE = 64;
export const ERRNO_PROTO = 65;
export const ERRNO_PROTONOSUPPORT = 66;
export const ERRNO_PROTOTYPE = 67;
export const ERRNO_RANGE = 68;
export const ERRNO_ROFS = 69;
export const ERRNO_SPIPE = 70;
export const ERRNO_SRCH = 71;
export const ERRNO_STALE = 72;
export const ERRNO_TIMEDOUT = 73;
export const ERRNO_TXTBSY = 74;
export const ERRNO_XDEV = 75;
export const ERRNO_NOTCAPABLE = 76;

export const RIGHTS_FD_DATASYNC = 1 << 0;
export const RIGHTS_FD_READ = 1 << 1;
export const RIGHTS_FD_SEEK = 1 << 2;
export const RIGHTS_FD_FDSTAT_SET_FLAGS = 1 << 3;
export const RIGHTS_FD_SYNC = 1 << 4;
export const RIGHTS_FD_TELL = 1 << 5;
export const RIGHTS_FD_WRITE = 1 << 6;
export const RIGHTS_FD_ADVISE = 1 << 7;
export const RIGHTS_FD_ALLOCATE = 1 << 8;
export const RIGHTS_PATH_CREATE_DIRECTORY = 1 << 9;
export const RIGHTS_PATH_CREATE_FILE = 1 << 10;
export const RIGHTS_PATH_LINK_SOURCE = 1 << 11;
export const RIGHTS_PATH_LINK_TARGET = 1 << 12;
export const RIGHTS_PATH_OPEN = 1 << 13;
export const RIGHTS_FD_READDIR = 1 << 14;
export const RIGHTS_PATH_READLINK = 1 << 15;
export const RIGHTS_PATH_RENAME_SOURCE = 1 << 16;
export const RIGHTS_PATH_RENAME_TARGET = 1 << 17;
export const RIGHTS_PATH_FILESTAT_GET = 1 << 18;
export const RIGHTS_PATH_FILESTAT_SET_SIZE = 1 << 19;
export const RIGHTS_PATH_FILESTAT_SET_TIMES = 1 << 20;
export const RIGHTS_FD_FILESTAT_GET = 1 << 21;
export const RIGHTS_FD_FILESTAT_SET_SIZE = 1 << 22;
export const RIGHTS_FD_FILESTAT_SET_TIMES = 1 << 23;
export const RIGHTS_PATH_SYMLINK = 1 << 24;
export const RIGHTS_PATH_REMOVE_DIRECTORY = 1 << 25;
export const RIGHTS_PATH_UNLINK_FILE = 1 << 26;
export const RIGHTS_POLL_FD_READWRITE = 1 << 27;
export const RIGHTS_SOCK_SHUTDOWN = 1 << 28;

export class Iovec {
  //@ts-ignore strictPropertyInitialization
  buf: number;
  //@ts-ignore strictPropertyInitialization
  buf_len: number;

  static read_bytes(view: DataView, ptr: number): Iovec {
    const iovec = new Iovec();
    iovec.buf = view.getUint32(ptr, true);
    iovec.buf_len = view.getUint32(ptr + 4, true);
    return iovec;
  }

  static read_bytes_array(
    view: DataView,
    ptr: number,
    len: number
  ): Array<Iovec> {
    const iovecs = [];
    for (let i = 0; i < len; i++) {
      iovecs.push(Iovec.read_bytes(view, ptr + 8 * i));
    }
    return iovecs;
  }
}

export class Ciovec {
  //@ts-ignore strictPropertyInitialization
  buf: number;
  //@ts-ignore strictPropertyInitialization
  buf_len: number;

  static read_bytes(view: DataView, ptr: number): Ciovec {
    const iovec = new Ciovec();
    iovec.buf = view.getUint32(ptr, true);
    iovec.buf_len = view.getUint32(ptr + 4, true);
    return iovec;
  }

  static read_bytes_array(
    view: DataView,
    ptr: number,
    len: number
  ): Array<Ciovec> {
    const iovecs = [];
    for (let i = 0; i < len; i++) {
      iovecs.push(Ciovec.read_bytes(view, ptr + 8 * i));
    }
    return iovecs;
  }
}

export const WHENCE_SET = 0;
export const WHENCE_CUR = 1;
export const WHENCE_END = 2;

export const FILETYPE_UNKNOWN = 0;
export const FILETYPE_BLOCK_DEVICE = 1;
export const FILETYPE_CHARACTER_DEVICE = 2;
export const FILETYPE_DIRECTORY = 3;
export const FILETYPE_REGULAR_FILE = 4;
export const FILETYPE_SOCKET_DGRAM = 5;
export const FILETYPE_SOCKET_STREAM = 6;
export const FILETYPE_SYMBOLIC_LINK = 7;

export class Dirent {
  d_next: bigint;
  d_ino = 0n;
  d_namlen: number;
  d_type: number;
  dir_name: Uint8Array;

  constructor(next_cookie: bigint, name: string, type: number) {
    const encoded_name = new TextEncoder().encode(name);

    this.d_next = next_cookie;
    this.d_namlen = encoded_name.byteLength;
    this.d_type = type;
    this.dir_name = encoded_name;
  }

  head_length(): number {
    return 24;
  }

  name_length(): number {
    return this.dir_name.byteLength;
  }

  write_head_bytes(view: DataView, ptr: number) {
    view.setBigUint64(ptr, this.d_next, true);
    view.setBigUint64(ptr + 8, this.d_ino, true);
    view.setUint32(ptr + 16, this.dir_name.length, true); // d_namlen
    view.setUint8(ptr + 20, this.d_type);
  }

  write_name_bytes(view8: Uint8Array, ptr: number, buf_len: number) {
    view8.set(
      this.dir_name.slice(0, Math.min(this.dir_name.byteLength, buf_len)),
      ptr
    );
  }
}

export const ADVICE_NORMAL = 0;
export const ADVICE_SEQUENTIAL = 1;
export const ADVICE_RANDOM = 2;
export const ADVICE_WILLNEED = 3;
export const ADVICE_DONTNEED = 4;
export const ADVICE_NOREUSE = 5;

export const FDFLAGS_APPEND = 1 << 0;
export const FDFLAGS_DSYNC = 1 << 1;
export const FDFLAGS_NONBLOCK = 1 << 2;
export const FDFLAGS_RSYNC = 1 << 3;
export const FDFLAGS_SYNC = 1 << 4;

export class Fdstat {
  fs_filetype: number;
  fs_flags: number;
  fs_rights_base = 0n;
  fs_rights_inherited = 0n;

  constructor(filetype: number, flags: number) {
    this.fs_filetype = filetype;
    this.fs_flags = flags;
  }

  write_bytes(view: DataView, ptr: number) {
    view.setUint8(ptr, this.fs_filetype);
    view.setUint16(ptr + 2, this.fs_flags, true);
    view.setBigUint64(ptr + 8, this.fs_rights_base, true);
    view.setBigUint64(ptr + 16, this.fs_rights_inherited, true);
  }
}

export const FSTFLAGS_ATIM = 1 << 0;
export const FSTFLAGS_ATIM_NOW = 1 << 1;
export const FSTFLAGS_MTIM = 1 << 2;
export const FSTFLAGS_MTIM_NOW = 1 << 3;

export const OFLAGS_CREAT = 1 << 0;
export const OFLAGS_DIRECTORY = 1 << 1;
export const OFLAGS_EXCL = 1 << 2;
export const OFLAGS_TRUNC = 1 << 3;

export class Filestat {
  dev = 0n;
  ino = 0n;
  filetype: number;
  nlink = 0n;
  size: bigint;
  atim = 0n;
  mtim = 0n;
  ctim = 0n;

  constructor(filetype: number, size: bigint) {
    this.filetype = filetype;
    this.size = size;
  }

  write_bytes(view: DataView, ptr: number) {
    view.setBigUint64(ptr, this.dev, true);
    view.setBigUint64(ptr + 8, this.ino, true);
    view.setUint8(ptr + 16, this.filetype);
    view.setBigUint64(ptr + 24, this.nlink, true);
    view.setBigUint64(ptr + 32, this.size, true);
    view.setBigUint64(ptr + 38, this.atim, true);
    view.setBigUint64(ptr + 46, this.mtim, true);
    view.setBigUint64(ptr + 52, this.ctim, true);
  }
}

export const EVENTTYPE_CLOCK = 0;
export const EVENTTYPE_FD_READ = 1;
export const EVENTTYPE_FD_WRITE = 2;

export const EVENTRWFLAGS_FD_READWRITE_HANGUP = 1 << 0;

export const SUBCLOCKFLAGS_SUBSCRIPTION_CLOCK_ABSTIME = 1 << 0;

export const SIGNAL_NONE = 0;
export const SIGNAL_HUP = 1;
export const SIGNAL_INT = 2;
export const SIGNAL_QUIT = 3;
export const SIGNAL_ILL = 4;
export const SIGNAL_TRAP = 5;
export const SIGNAL_ABRT = 6;
export const SIGNAL_BUS = 7;
export const SIGNAL_FPE = 8;
export const SIGNAL_KILL = 9;
export const SIGNAL_USR1 = 10;
export const SIGNAL_SEGV = 11;
export const SIGNAL_USR2 = 12;
export const SIGNAL_PIPE = 13;
export const SIGNAL_ALRM = 14;
export const SIGNAL_TERM = 15;
export const SIGNAL_CHLD = 16;
export const SIGNAL_CONT = 17;
export const SIGNAL_STOP = 18;
export const SIGNAL_TSTP = 19;
export const SIGNAL_TTIN = 20;
export const SIGNAL_TTOU = 21;
export const SIGNAL_URG = 22;
export const SIGNAL_XCPU = 23;
export const SIGNAL_XFSZ = 24;
export const SIGNAL_VTALRM = 25;
export const SIGNAL_PROF = 26;
export const SIGNAL_WINCH = 27;
export const SIGNAL_POLL = 28;
export const SIGNAL_PWR = 29;
export const SIGNAL_SYS = 30;

export const RIFLAGS_RECV_PEEK = 1 << 0;
export const RIFLAGS_RECV_WAITALL = 1 << 1;

export const ROFLAGS_RECV_DATA_TRUNCATED = 1 << 0;

export const SDFLAGS_RD = 1 << 0;
export const SDFLAGS_WR = 1 << 1;

export const PREOPENTYPE_DIR = 0;

export class PrestatDir {
  pr_name: Uint8Array;

  constructor(name: string) {
    this.pr_name = new TextEncoder().encode(name);
  }

  write_bytes(view: DataView, ptr: number) {
    view.setUint32(ptr, this.pr_name.byteLength, true);
  }
}

export class Prestat {
  //@ts-ignore strictPropertyInitialization
  tag: number;
  //@ts-ignore strictPropertyInitialization
  inner: PrestatDir;

  static dir(name: string): Prestat {
    const prestat = new Prestat();
    prestat.tag = PREOPENTYPE_DIR;
    prestat.inner = new PrestatDir(name);
    return prestat;
  }

  write_bytes(view: DataView, ptr: number) {
    view.setUint32(ptr, this.tag, true);
    this.inner.write_bytes(view, ptr + 4);
  }
}
