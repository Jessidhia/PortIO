package PortIO;

use v5.10;
use strict;
use warnings;

BEGIN {
	our $VERSION = v0.1_1;
};

=head1 NAME

PortIO - semitransparent unicode filename support on windows

=head1 DESCRIPTION

This module implements several wrapper functions for file handling. They are
intended to be used as a workaround when writing cross-platform code that needs
to run on windows, since Perl doesn't support unicode filenames there. All
functions defined here were tested to work on both windows and linux.

These functions also always try to use long absolute paths on windows, which
allows have a length of up to 65536 characters. It can convert relative
paths to absolute ones.

=head1 FUNCTIONS

All functions expect to receive utf8 strings as arguments and return utf8
strings.

=head2 C<printout($)>

Prints a single string to STDOUT.

=head2 C<printerr($)>

Prints a single string to STDERR.

=head2 C<move($source,$destination)>

Moves a file from $source to $destination.

=head2 C<mkpath($path)>

Creates the $path folder with all needed parent folders.

=head2 C<rmpath($path)>

Recursively deletes $path.

=head2 C<stat($path)>

Returns a L<File::stat> object with information from the file pointed
by $path. Follow symlinks on unix, not yet on windows.

=head2 C<lstat($path)>

Does the same as L</C<stat($path)>>, but doesn't follow symlinks.

=head2 C<file_exists($path)>

Equivalent to L<-e|-X>, returns true if $path exists.

=head2 C<file_size($path)>

Equivalent to L<-s|-X>, returns the file size in bytes.

=head2 C<is_dir($path)>

Equivalent to L<-d|-X>, returns 1 if $path is a directory.

=head2 C<is_file($path)>

Equivalent to L<-f|-X>, returns 1 if $path is a file.

=head2 C<file_open($mode, $path)>

Wrapper around three-argument L<open>(). Returns the opened PerlIO handle that
can be used in any other regular perl function.
Currently supports only the '>', '>>', '<', '+<' and '+>' modes.
Also supports PerlIO layers.

=head2 C<file_unlink($file)>

Equivalent to L<unlink>, unlinks a file.

=head2 C<dir_rm($dir)>

Equivalent to L<rmdir>, Removes a directory if it is empty.

=head2 C<diropen($dir)>

Wrapper around L<opendir>. Opens a handle to $dir, returns the handle. Note
that the returned handle is only compatible with PerlIO::dirread and
PerlIO::dirclose.

=head2 C<dirread($handle)>

Wrapper around L<readdir>. Returns the next child item in the directory. The
returned item is always the basename, so you need to prepend it with the
directory path if you intend to do anything with a child item.

NOTE: This function expects the the filesystem to use UTF-8 filenames on non-windows.

=head2 C<dirclose($handle)>

Closes a $handle previously returned by L</"diropen">. This also makes the $handle
invalid.

=cut

use Symbol;
use Encode;
use Fcntl ':mode';

require File::stat;

BEGIN {
	use Exporter ();
	our (@ISA, @EXPORT, @EXPORT_OK, %EXPORT_TAGS);

	@ISA = qw(Exporter);

	@EXPORT = qw(*printout *printerr *move *mkpath *rmpath
	             *file_exists *file_size *is_dir *is_file
	             *file_open *file_unlink *dir_rm
	             *diropen *dirread *dirclose);
};
our @EXPORT_OK;

### Windows Unicode support wrappers
if ($^O =~ /MSWin/) {
	require Win32API::File;
	Win32API::File->import qw(:FuncW :Func :MOVEFILE_ :GENERIC_ :FILE_
	                          :FILE_SHARE_ :FILE_TYPE_ :FILE_ATTRIBUTE_ :Misc);
	require Win32::API; Win32::API->import;
	require Win32API::File::Time;

	# Enable output of UTF-8 text

	my $WriteConsoleW = Win32::API->new('kernel32.dll', 'WriteConsoleW',
	                                    'IPIPP', 'I') or die "WriteConsoleW: $^E";

	my $stdout = GetOsFHandle(*STDOUT);
	my $stderr = GetOsFHandle(*STDERR);

	# Use Win32::API to load needed functions not defined by Win32API::File

	my $FindFirstFileW = Win32::API->new('kernel32.dll', 'FindFirstFileW',
	                                     'PP', 'N') or die "FindFirstFileW: $^E";
	my $FindNextFileW  = Win32::API->new('kernel32.dll', 'FindNextFileW',
	                                     'NP', 'I') or die "FindNextFileW: $^E";
	my $FindClose      = Win32::API->new('kernel32.dll', 'FindClose',
	                                     'N', 'I') or die "FileClose: $^E";

	my $CreateDirectoryW = Win32::API->new('kernel32.dll',
	    'BOOL CreateDirectoryW(LPCWSTR lpPathName, LPVOID lpSecurityAttributes);')
	        or die "CreateDirectoryW: $^E";

	my $RemoveDirectoryW = Win32::API->new('kernel32.dll',
	    'BOOL RemoveDirectoryW(LPCWSTR lpPathName);') or die "RemoveDirectoryW: $^E";

	my $GetFullPathNameW = Win32::API->new('kernel32.dll',
	    'DWORD GetFullPathNameW(LPCWSTR lpFileName, DWORD nBufferLength, '.
	    'LPWSTR lpBuffer, LPWSTR *lpFilePart);') or die "GetFullPathNameW: $^E";

	my $GetFileTime = Win32::API->new('kernel32.dll', 'GetFileTime', [qw{N P P P}], 'I') or
	        die "GetFileTime: $^E";

	my $GetCurrentDirectoryW = Win32::API->new('kernel32.dll',
	    'DWORD GetCurrentDirectoryW(DWORD nBufferLength, LPWSTR lpBuffer);') or
	        die "GetCurrentDirectoryW: $^E";

	my $utf16 = find_encoding("UTF-16LE");

	my $abs_fname = sub {
		my $str = shift;
		return undef unless defined($str);
		# Replaces special start sequences with meaningful values
		# . = current working directory
		# .. = parent of the cwd
		# / = root directory of the cwd
		if ($str =~ /^[\.\/]\.?/) {
			my $len = $GetCurrentDirectoryW->Call(0, []);
			my $dir = " " x ($len*2);
			$len = $GetCurrentDirectoryW->Call($len, $dir);
			$dir = substr($dir, 0, $len*2);
			$dir = $utf16->decode($dir);
			my @path = split(/\\/, $dir);
			if ($str =~ /^\.\./) {
				$dir = join("\\", @path[0..$#path-1]);
				$str =~ s/^\.\./$dir/;
			} elsif ($str =~ /^\./) {
				$str =~ s/^\./$dir/;
			} else {
				$str =~ s/^\//$path[0]\//;
			}
		}
		$str =~ s/\//\\/g;
		my @inpath = split(/\\/, $str);
		my @path = ();
		# Remove . and .. from the path by resolving them
		foreach (@inpath) {
			if ($_ eq '.') {
				# Do nothing (stay on the current dir)
			} elsif ($_ eq '..') {
				# Remove the last element (current dir) to get to the parent dir
				pop @path unless @path == 1;
			} else {
				push @path, $_;
			}
		}
		$str = join('\\', @path);
		# Make sure the drive letter has a \\ right after it
		$str .= "\\" if $str =~ /^\w:[^\\]$/;
		my $in = "$str";
		$in = "\\\\?\\$in" unless $in =~ m/^\\\\?\\/;
		my $orig = $utf16->encode($in);
		$in = $orig . "\0\0";
		my $len = 0;
		my $out = " " x (($len = $GetFullPathNameW->Call($in, 0, [], [])) * 2);
		$len = $GetFullPathNameW->Call($in, $len, $out, []);
		$out = substr($out, 0, $len*2);
		if ($out eq $orig) {
			$in = $utf16->encode("$str\0");
			$out = " " x (($len = $GetFullPathNameW->Call($in, 0, [], [])) * 2);
			$len = $GetFullPathNameW->Call($in, $len, $out, []);
			$out = substr($out, 0, $len*2);
			$out = $utf16->encode("\\\\?\\" . $utf16->decode($out) . "\0");
			return $out;
		} else {
			return $out;
		}
		return $utf16->encode("$str\0");
	};
	my $print = sub {
		my $fd = shift;
		my $str = join(" ",@_);
		my $len = length($str);
		$str = $utf16->encode($str);
		my $count = 0;
		return $count if $WriteConsoleW->Call($fd, $str, $len, $count, []);
		return 0;
	};

	*printout = sub {
		$print->($stdout, shift);
	};
	*printerr = sub {
		$print->($stderr, shift);
	};
	*move = sub {
		my ($src, $dest) = @_;
		MoveFileExW($abs_fname->("$src"), $abs_fname->("$dest"),
		            fileConstant("MOVEFILE_COPY_ALLOWED")|
		            fileConstant("MOVEFILE_WRITE_THROUGH"));
	};
	*mkpath = sub {
		my $path = shift;
		my $p = $path;
		unless (file_exists($path)) {
			$! = 0;
			unless ($CreateDirectoryW->Call($abs_fname->("$p"), undef)) {
				my $parent = $path;
				$parent =~ s/\/[^\/]*$//;
				if ($parent eq $path) {
					return undef;
				}
				unless (mkpath($parent)) {
					return undef;
				}
				# Parent made, try again making the child
				unless ($CreateDirectoryW->Call($abs_fname->("$p"), undef)) {
					return undef;
				}
			}
		}
		return $path;
	};
	*rmpath = sub {
		my $path = shift;
		my $dir = diropen($path);
		while ($_ = dirread($dir)) {
			$_ = decode("UTF-8", $_, 1);
			unless($_ eq '.' or $_ eq '..') {
				$_ = "$path/$_";
				if (is_dir($_)) {
					rmpath($_) or (dirclose($dir) and return 0);
				} else {
					file_unlink($_) or (dirclose($dir) and return 0);
				}
			}
		}
		dirclose($dir);
		dir_rm($path) or return 0;
		return 1;
	};
	*lstat = sub {
	    my $arg = shift;
		my $attrib = GetFileAttributesW($abs_fname->("$arg"));
		if ($attrib == INVALID_FILE_ATTRIBUTES()) {
			return undef;
		}
		my $handle;

		# TODO: figure out how to get the 1st field. 2nd doesn't exist on windows.
		my @stat = (0, 0);
		my $mode = S_IRWXU | S_IRWXG | S_IRWXO;
		$mode &= ~(S_IWUSR | S_IWGRP | S_IWOTH) if ($attrib & FILE_ATTRIBUTE_READONLY());
		if ($attrib & FILE_ATTRIBUTE_DIRECTORY()) {
			$mode |= S_IFDIR;
		} else {
			$handle = CreateFileW($abs_fname->("$arg"), 0,
			                      FILE_SHARE_READ() | FILE_SHARE_WRITE(), [],
			                      OPEN_EXISTING(), 0, []);
			my $type = GetFileType($handle);
			if ($type & FILE_TYPE_DISK()) {
				$mode |= S_IFREG;
			} else {
				warn "Unknown filetype $type";
			}
		}
		push @stat, $mode;
		# TODO: figure out the next 4 fields.
		push @stat, (1, 0, 0, 6);
		if ($handle) {
			push @stat, (getFileSize($handle)->numify());
			my ($atime, $mtime, $ctime);
			$atime = $mtime = $ctime = pack 'LL', 0, 0;
			$GetFileTime->Call($handle, $ctime, $atime, $mtime);
			push @stat, (Win32API::File::Time::_filetime_to_perltime($atime),
			             Win32API::File::Time::_filetime_to_perltime($mtime),
			             Win32API::File::Time::_filetime_to_perltime($ctime));
		} else {
			# TODO: implement ^ for directories
			push @stat, (0, 0, 0, 0);
		}
		# Note: the last two fields are undefined on windows.
		my $st = File::stat::populate(@stat);
		return $st if $st;
		return undef;
	};
	*stat = sub {
		# TODO: actually follow symlinks
		return PortIO::lstat(shift);
	};
	*file_exists = sub{
		my $p = shift;
		return undef unless $p;
		return GetFileAttributesW($abs_fname->("$p")) != fileConstant("INVALID_FILE_ATTRIBUTES");
	};
	*file_size = sub{
		my $p = shift;
		return 0 unless file_exists($p);
		my $handle = CreateFileW($abs_fname->("$p"), 0, fileConstant("FILE_SHARE_READ") |
		                                                fileConstant("FILE_SHARE_WRITE"), [],
		                                                fileConstant("OPEN_EXISTING"), 0, []);
		my $size = getFileSize($handle)->numify();
		CloseHandle($handle);
		return $size;
	};
	*is_dir = sub {
		my $p = shift;
		return 0 unless file_exists($p);
		return !!(GetFileAttributesW($abs_fname->("$p")) & FILE_ATTRIBUTE_DIRECTORY());
	};
	*is_file = sub {
		my $p = shift;
		return 0 unless file_exists($p);
		my $handle = CreateFileW($abs_fname->("$p"), 0, fileConstant("FILE_SHARE_READ") |
		                                                fileConstant("FILE_SHARE_WRITE"), [],
		                                                fileConstant("OPEN_EXISTING"), 0, []);
		my $is_file = GetFileType($handle) & FILE_TYPE_DISK();
		CloseHandle($handle);
		return $is_file;
	};
	*file_open = sub {
		my $mode = shift;
		my $file = shift;
		$file = $abs_fname->("$file");
		my @filters = split(':',$mode);
		$mode = shift @filters;
		die "File mode $mode not implemented" unless
		    scalar(grep { $mode eq $_ } ('<', '>', '>>', '+<', '+>'));

		my ($flags, $share, $create) = (GENERIC_READ(), FILE_SHARE_READ(), undef);
		if ($mode eq '<') {
			$share |= FILE_SHARE_WRITE();
		} elsif ($mode eq '>' or $mode eq '>>') {
			$flags = GENERIC_WRITE();
		} else {
			$flags |= GENERIC_WRITE();
		}
		if ($mode eq '<') {
			$create = OPEN_EXISTING();
		} elsif ($mode eq '>' or $mode eq '+>') {
			$create = CREATE_ALWAYS();
		} else {
			$create = OPEN_ALWAYS();
		}
		my $handle = CreateFileW($file, $flags, $share, [], $create, 0, []);
		unless ($handle) {
			my $f = $utf16->decode($file);
			printout("Error opening $f: $^E\n");
			die;
		}
		setFilePointer($handle, 0, FILE_END()) if $mode eq '>>';

		my $f = gensym();
		OsFHandleOpen($f, $handle, ($flags & GENERIC_READ() ? 'r' : '') .
		                           ($flags & GENERIC_WRITE() ? 'w' : '') .
		                           ($mode eq '>>' ? 'a' : '')) and $f or die "$!";

		if (@filters) {
			binmode($f, ":". join(":", @filters));
		}

		return $f;
	};
	*file_unlink = sub {
		DeleteFileW($abs_fname->(shift));
	};
	*dir_rm = sub {
		$RemoveDirectoryW->Call($abs_fname->(shift));
	};
	*diropen = sub {
		my $path = $abs_fname->(($_ = shift) . "/*");
		my $FileInfo = " " x 1140;
		my $handle = $FindFirstFileW->Call($path, $FileInfo);

		return {
			handle => $handle,
			file_info => $FileInfo
		};
	};
	*dirread = sub {
		my $entry = shift;
		return undef unless exists($entry->{file_info});
		my $FileInfo = $entry->{file_info};
		my ($attrib, $filename);

		$filename = unpack("x44A520", $FileInfo);
		$filename = $utf16->decode("$filename\0", 1);

		$FileInfo = "\0" x (1140); # Clear the struct

		if ($FindNextFileW->Call($entry->{handle}, $FileInfo)) {
			$entry->{file_info} = $FileInfo;
		} else {
			delete $entry->{file_info};
		}

		my @path = split(/\\/, $filename);
		return $path[$#path];
	};
	*dirclose = sub {
		$FindClose->Call($_[0]->{handle});
		delete $_[0]->{handle};
		delete $_[0]->{file_info};
	}
} else {
	require File::Copy;
	require File::Path;
	$|++;
	*printout = sub { STDOUT->print(shift); };
	*printerr = sub { STDERR->print(shift); };
	*move = sub { File::Copy::move(shift, shift); };
	*mkpath = sub { File::Path::mkpath(shift); };
	*rmpath = sub { File::Path::rmpath(shift); };
	*lstat = sub { File::stat::lstat(shift); };
	*stat = sub { File::stat::stat(shift); };
	*file_exists = sub { -e shift; };
	*file_size = sub { -s shift; };
	*is_dir = sub { -d shift; };
	*is_file = sub { -f shift; };
	*file_open = sub {
		my ($mode, $file) = @_;
		my @filters = split(':',$mode);
		$mode = shift @filters;
		die "File mode $mode not implemented" unless
		    scalar(grep { $mode eq $_ } ('<', '>', '>>', '+<', '+>'));
		if (@filters) {
			$mode .= ":" . join(":", @filters);
		}
		my $f;
		open ($f, $mode, $file);
		return $f;
	};
	*file_unlink = sub { unlink(shift); };
	*dir_rm = sub { rmdir(shift); };
	*diropen = sub { opendir $_, shift; return $_; };
	*dirread = sub { decode("UTF-8", readdir(shift), 1); };
	*dirclose = sub { closedir shift; }
}

1;
