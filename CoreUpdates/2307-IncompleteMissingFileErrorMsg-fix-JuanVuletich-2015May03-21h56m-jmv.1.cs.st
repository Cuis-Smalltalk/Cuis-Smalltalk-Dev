
'From Cuis 4.2 of 25 July 2013 [latest update: #2306] on 3 May 2015 at 10:06:29.758809 pm'!

!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 5/3/2015 22:05'!
openSourcesAndChanges
	"Open the changes and sources files and install them in SourceFiles. Inform the user of problems regarding write permissions or Lf/CrLf mixups."
	"Note: SourcesName and imageName are full paths; changesName is a  
	local name."
	| sourcesPathName changesPathName sources changes msg wmsg |
	msg _ 'Cuis cannot locate XfileRef
Please check that the file is named properly and is in the
same directory as this image.'.
	wmsg _ 'Cuis cannot write to XfileRef.

Please check that you have write permission for this file.

You won''t be able to save this image correctly until you fix this.'.

	"Do not open source files if internalized (i.e. notNil)"
	sources _ SourceFiles at: 1.
	sources ifNil: [
		sourcesPathName _ Smalltalk defaultSourcesName.
		sources _ FileStream oldFileOrNoneNamed: sourcesPathName.
		sources ifNil: [
			sourcesPathName _ Smalltalk alternativeSourcesName.
			sources _ FileStream oldFileOrNoneNamed: sourcesPathName ]].
	(sources isNil and: [ Preferences valueOfFlag: #warnIfNoSourcesFile ])
		ifTrue: [
			Smalltalk platformName = 'Mac OS' ifTrue: [
				msg _ msg , String newLineString, 'Make sure the sources file is not an Alias.'].
			self inform: (msg copyReplaceAll: 'XfileRef' with: 'the sources file named ' , sourcesPathName) ].

	changesPathName _ Smalltalk defaultChangesName.
	"Do not open source files if internalized (i.e. notNil)"
	changes _ (SourceFiles at: 2) ifNil: [ FileStream oldFileNamed: changesPathName ].
	(changes isNil and: [ Preferences valueOfFlag: #warnIfNoChangesFile ])
		ifTrue: [self inform: (msg copyReplaceAll: 'XfileRef' with: 'the changes file named ' , changesPathName)].

	((Preferences valueOfFlag: #warnIfNoChangesFile) and: [changes notNil]) ifTrue: [
		changes isReadOnly ifTrue: [
			self inform: (wmsg copyReplaceAll: 'XfileRef' with: 'the changes file named ' , changesPathName)].

		((changes next: 200) includesSubString: String crlfString) ifTrue: [
			self inform: 'The changes file named ' , changesPathName , '
has been injured by an unpacking utility.  Lfs were changed to CrLfs.
Please set the preferences in your decompressing program to 
"do not convert text files" and unpack the system again.']].

	SourceFiles _ Array with: sources with: changes! !

