'From Cuis 5.0 of 7 November 2016 [latest update: #3168] on 9 September 2017 at 12:50:57 am'!

!FileList commentStamp: '<historical>' prior: 0!
I am model that can be used to navigate the host file system. By omitting the volume list, file list, and template panes from the view, I can also be used as the model for an editor on an individual file.

The FileList provides a dynamic extension mechanism.  To extend FileList functionality, tools should implement the following class-side method (look for implementors in the image):

#fileReaderServicesForFile:suffix: (appropriate services for given file, takes a file name and a lowercased suffix)

This method returns a collection of SimpleServiceEntry instances.  These contain a class, a menu label and a method selector having one argument.  They may also provide separate button labels and description.

The argument to the specified method will be a string representing the full name of a file when one is selected or the file list itself when there is no selected file.

There is a testSuite called FileListTest that presents some examples. 

Stef (I do not like really this distinction passing always a file list could be better)


Old Comments: 


FileLists can now see FTP servers anywhere on the net.  In the volume list menu: 
fill in server info...		Gives you a form to register a new ftp server you want to use.
open server...		Choose a server to connect to.
local disk			Go back to looking at your local volume.


Still undone (you can contribute code):
[ ] Using a Proxy server to get out through a firewall.  What is the convention for proxy servers with FTP?
[ ] Fill in the date and size info in the list of remote files.  Allow sorting by it.  New smarts needed in (ServerDirectory fileNameFormattedFrom:sizePad:sortMode:).
[ ] Currently the FileList has no way to delete a directory.  Since you can't select a directory without going into it, it would have to be deleting the current directory.  Which would usually be empty.!

