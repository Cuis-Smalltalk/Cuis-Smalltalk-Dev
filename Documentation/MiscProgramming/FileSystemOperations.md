For filesystem operations the original Squeak class ``FileDirectory`` and others have been replaced in 2015 by an adapted version of [FileMan](http://wiki.squeak.org/squeak/6333) (https://github.com/mumez/FileMan).
The class ``FileDirectory`` is no longer available.

Some typical FileMan code examples:

The expression

    'dirName' asDirectoryEntry / 'subdir'
    
answers 

     a DirectoryEntry

The new operator #//

    'dirName' asDirectoryEntry // 'filename'

answers 
 
      an FileEntry.
      
To set the desktop background is an example for the FileMan messages #asFileEntry and #binaryContents

    | filename |
    filename := 'Pluto.png'.
    self runningWorld backgroundImageData: filename asFileEntry binaryContents.
