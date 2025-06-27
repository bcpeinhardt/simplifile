-ifndef(SIMPLIFILE_FILE_HRL_).

-define(SIMPLIFILE_FILE_HRL_, 1).

%%--------------------------------------------------------------------------

-record(simplifile_file_info,
        {size :: non_neg_integer() | undefined,  % Size of file in bytes.
         mode :: non_neg_integer() | undefined,
         % File permissions.  On Windows,
         % the owner permissions will be
         % duplicated for group and user.
         links :: non_neg_integer() | undefined,
         inode :: non_neg_integer() | undefined,  % Inode number for file.
         uid :: non_neg_integer() | undefined,   % User id for owner.
         gid :: non_neg_integer() | undefined, % Group id for owner.
         major_device :: non_neg_integer() | undefined,
         % Identifies the file system (Unix),
         % or the drive number (A: = 0, B: = 1)
         % (Windows).
         %% The following are Unix specific.
         %% They are set to zero on other operating systems.
         atime :: file:date_time() | non_neg_integer() | undefined,
         % The local time the file was last read:
         % {{Year, Mon, Day}, {Hour, Min, Sec}}.
         % atime, ctime, mtime may also be unix epochs()
         mtime :: file:date_time() | non_neg_integer() | undefined,
         % The local time the file was last written.
         ctime ::
                 file:date_time() |
                 non_neg_integer() |
                 undefined}).        % The interpretation of this time field
                                     % is dependent on operating system.
                                     % On Unix it is the last time the file
                                     % or the inode was changed.  On Windows,
                                     % it is the creation time.

%%--------------------------------------------------------------------------
-endif.
