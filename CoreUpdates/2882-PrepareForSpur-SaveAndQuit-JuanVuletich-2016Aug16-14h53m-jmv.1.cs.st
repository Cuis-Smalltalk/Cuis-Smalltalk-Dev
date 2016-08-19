'From Cuis 4.2 of 25 July 2013 [latest update: #2881] on 16 August 2016 at 2:54:45 pm'!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
| p systemVersion fileName n |
UseZCompiler _ false.
p _ Processor activeProcess.
[
	p terminate.
	ProjectX newProcessIfUIX: p.
	WorldState addDeferredUIMessage: [
		((PopUpMenu labels: ' OK ') startUpWithCaption: '
This image is now ready to be converted to Spur format.

The image will now be saved and quitted.
You can start the new image and update again, to have an updated non-Spur image.
Or you can run the Spur bootstrap on it, and then start it with a Spur 32bit VM to have an updated Spur image.').
		systemVersion _ SystemVersion current.
		n _ (systemVersion highestUpdate+1) printString.
		systemVersion registerUpdate: n. "+1 because this very change set will not complete the installation normally..."
		('Installed ChangeSet: ', n) print.
		fileName _ String streamContents: [ :strm |
			strm
				nextPutAll: 'Cuis';
				print: systemVersion versionMajor;
				nextPut: $.;
				print: systemVersion versionMinor;
				nextPut: $-;
				print: systemVersion highestUpdate ].
		Smalltalk saveAs: fileName, '-SpurReady.image' andQuit: true clearAllClassState: true not
		]
] forkAt: 42.
!
