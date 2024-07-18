'From Cuis7.1 [latest update: #6523] on 18 July 2024 at 11:28:17 am'!

!SystemDictionary methodsFor: 'startup' stamp: 'jmv 7/18/2024 11:08:44'!
displayCommandLineUsageOn: aStream
	"Display command line usage on standard out."

	[ aStream
		nextPutAll: 'Command line options:'; newLine;
		nextPutAll: '  -h Display this help message and quit'; newLine;
		nextPutAll: '  -v Display the system information, containing the version and quit'; newLine;
		nextPutAll: '  -q Just quit'; newLine;
		nextPutAll: '  -e Do not ignore exceptions, but open a Debugger'; newLine;
		nextPutAll: '  -u Install updates from CoreUpdates directory'; newLine;
		nextPutAll: '  -r <feature> Require <feature>'; newLine;
		nextPutAll: '  -d <code> Evaluate <code>'; newLine;
		nextPutAll: '  -l <codeFile> Load e.g. file in <codeFile>'; newLine;
		nextPutAll: '  -s <scriptFile> Evaluate code in <scriptFile>'; newLine;
		nextPutAll: '  -ud <directory> Set User Directory to be <directory>'; newLine;
		nextPutAll: '  -udIsBase Set User Directory to be Cuis Base directory'; newLine;
		nextPutAll: '  -- end of options; stop processing options'; newLine;
		newLine;
		nextPutAll: 'Arguments for scripts, code files, packages (and <code>):'; newLine;
		nextPutAll: '  are available in ''Smalltalk startUpScriptArguments'''; newLine;
		newLine;
		nextPutAll: 'Multiple options:'; newLine;
		nextPutAll: '  any option can be passed multiple times'; newLine;
		flush ]
	on: UnhandledError
	do: [ :exception | exception return ].! !

!SystemDictionary methodsFor: 'startup' stamp: 'jmv 7/18/2024 11:08:57'!
displaySystemInformationStringOn: aStream
	"Display the system information string on standard output."

	[ aStream
		nextPutAll: 'System information:';		newLine;
		nextPutAll: self systemInformationString;		newLine;
		flush ]
	on: UnhandledError
	do: [ :exception | exception return ].! !


!Transcript class methodsFor: 'preferred protocol' stamp: 'jmv 7/18/2024 11:10:01'!
clearStdout

	logToStdout ifTrue: [
		15 timesRepeat: [ self stdout newLine ].
		self stdout flush
	]! !

!Transcript class methodsFor: 'private' stamp: 'jmv 7/18/2024 11:10:35'!
writeToStdout: anEntry

	logToStdout ifTrue: [ self stdout nextPutAll: anEntry; flush ]! !

