-record(file_info, {
    size :: integer(),
    mode :: integer(),
    nlinks :: integer(),
    inode :: integer(),
    user_id :: integer(),
    group_id :: integer(),
    dev :: integer(),
    atime_seconds :: integer(),
    mtime_seconds :: integer(),
    ctime_seconds :: integer()
}).
