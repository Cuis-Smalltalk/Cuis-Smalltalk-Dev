'From Cuis7.5 [latest update: #7415] on 14 August 2025 at 9:54:09 am'!

!SystemDictionary methodsFor: 'startup' stamp: 'jmv 8/14/2025 09:44:26'!
displayCommandLineUsageOn: aStream
	"Display command line usage on standard out."

	[ aStream
		nextPutAll: 'Command line options:'; newLine;
		nextPutAll: '  -h Display this help message and quit'; newLine;
		nextPutAll: '  -v Display the system information, containing the version and quit'; newLine;
		nextPutAll: '  -q Just quit'; newLine;
		nextPutAll: '  -e Do not ignore exceptions during startup, but open a Debugger'; newLine;
		nextPutAll: '  -u Install available updates from CoreUpdates directory'; newLine;
		nextPutAll: '  -r <feature> Require <feature>. Find and install a Cuis package file supplying it.'; newLine;
		nextPutAll: '  -d <code> Evaluate code. <code> must be a Smalltalk expression in double quotes. (No comments allowed)'; newLine;
		nextPutAll: '  -s <scriptFile> The file must contain a valid Smalltalk expression. Compile and evaluate it.'; newLine;
		nextPutAll: '  -l <codeFile> File in Smalltalk code. The file must be a valid ChangeSet file.'; newLine;
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

