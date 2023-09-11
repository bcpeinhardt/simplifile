-module(simplifile_erl).
-export([
    read_file/1,
    append_file/2, write_file/2, delete_file/1, delete_directory/1, recursive_delete/1,
    list_directory/1, make_directory/1, is_file/1, create_dir_all/1, rename_file/2
]).

-define(is_posix_error(Error),
    Error =:= eacces orelse Error =:= eagain orelse Error =:= ebadf orelse
    Error =:= ebadmsg orelse Error =:= ebusy orelse Error =:= edeadlk orelse
    Error =:= edeadlock orelse Error =:= edquot orelse Error =:= eexist orelse
    Error =:= efault orelse Error =:= efbig orelse Error =:= eftype orelse
    Error =:= eintr orelse Error =:= einval orelse Error =:= eio orelse
    Error =:= eisdir orelse Error =:= eloop orelse Error =:= emfile orelse
    Error =:= emlink orelse Error =:= emultihop orelse Error =:= enametoolong orelse
    Error =:= enfile orelse Error =:= enobufs orelse Error =:= enodev orelse
    Error =:= enolck orelse Error =:= enolink orelse Error =:= enoent orelse
    Error =:= enomem orelse Error =:= enospc orelse Error =:= enosr orelse
    Error =:= enostr orelse Error =:= enosys orelse Error =:= enotblk orelse
    Error =:= enotdir orelse Error =:= enotsup orelse Error =:= enxio orelse
    Error =:= eopnotsupp orelse Error =:= eoverflow orelse Error =:= eperm orelse
    Error =:= epipe orelse Error =:= erange orelse Error =:= erofs orelse
    Error =:= espipe orelse Error =:= esrch orelse Error =:= estale orelse
    Error =:= etxtbsy orelse Error =:= exdev
).

posix_result(Result) ->
    case Result of
        ok -> {ok, nil};
        {ok, Value} -> {ok, Value};
        {error, Reason} when ?is_posix_error(Reason) -> {error, Reason}
    end.

read_file(Filename) ->
    posix_result(file:read_file(Filename)).

write_file(Contents, Filename) ->
    posix_result(file:write_file(Filename, Contents)).

append_file(Contents, Filename) ->
    posix_result(file:write_file(Filename, Contents, [append])).

delete_file(Filename) ->
    posix_result(file:delete(Filename)).

make_directory(Dir) ->
    posix_result(file:make_dir(Dir)).

list_directory(Dir) ->
    case file:list_dir(Dir) of
        {ok, Filenames} ->
            {ok, [list_to_binary(Filename) || Filename <- Filenames]};
        {error, Reason} when ?is_posix_error(Reason) ->
            {error, Reason}
    end.

delete_directory(Dir) ->
    posix_result(file:del_dir(Dir)).

recursive_delete(Dir) ->
    posix_result(file:del_dir_r(Dir)).

is_file(Filename) ->
    not (file:read_file_info(Filename) == {error, enoent}) and not filelib: is_dir(Filename).

create_dir_all(Filename) ->
    posix_result(filelib:ensure_dir(Filename)).

rename_file(Source, Destination) ->
    posix_result(file:rename(Source, Destination)).
