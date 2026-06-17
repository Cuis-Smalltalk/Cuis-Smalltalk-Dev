'From Cuis7.9 [latest update: #8001] on 16 June 2026 at 5:44:06 pm'!

!ContextPart methodsFor: 'debugger access' stamp: 'jmv 16/Jun/2026 17:43:21'!
errorReportOn: strm
	"Write a detailed error report on the stack (above me) on a stream.  For both the error file, and emailing a bug report.  Suppress any errors while getting printStrings.  Limit the length."

	| cnt aContext startPos |
	DateAndTime nowUTC printNiceOn: strm.
	strm newLine; newLine.
	strm nextPutAll: 'VM: ';
		nextPutAll: Smalltalk platformName;
		nextPutAll: ' - ';
		nextPutAll: Smalltalk vmVersion;
		newLine.
	strm nextPutAll: 'Image: ';
		nextPutAll: Smalltalk versionString;
		nextPutAll: ' [';
		nextPutAll: Smalltalk lastUpdateString;
		nextPutAll: ']';
		newLine.
	strm newLine.
	
	"Note: The following is an open-coded version of ContextPart>>stackOfSize: since this method may be called during a low space condition and we might run out of space for allocating the full stack."
	cnt := 0.  startPos := strm position.
	aContext := self.
	[aContext notNil and: [(cnt := cnt + 1) < Debugger defaultDebugStackSize]] whileTrue: [
		aContext printDetails: strm.	"variable values"
		strm newLine.
		aContext := aContext sender].

	strm newLine; nextPutAll: '--- The full stack ---'; newLine.
	aContext := self.
	cnt := 0.
	[aContext == nil] whileFalse: [
		cnt := cnt + 1.
		cnt = Debugger defaultDebugStackSize ifTrue: [
			strm
				nextPutAll: ' - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -';
				newLine ].
		strm print: aContext; newLine.  "just class>>selector"	

		strm position > (startPos+40000) ifTrue: [
			strm nextPutAll: '...etc...'.
			^ self]. 	"exit early"
		cnt > 100 ifTrue: [
			strm nextPutAll: '-- and more not shown --'.
			^ self].
		aContext := aContext sender ].! !

!ContextPart methodsFor: 'debugger access' stamp: 'jmv 16/Jun/2026 17:42:33'!
shortErrorReportOn: strm
	"Write a short error report on the stack (above me) on a stream.  For both the error file, and emailing a bug report. "

	| cnt aContext |
	DateAndTime nowUTC printNiceOn: strm.
	strm newLine.
	aContext := self.
	cnt := 0.
	[aContext notNil and: [(cnt := cnt + 1) < Debugger defaultDebugStackSize]] whileTrue: [
		strm print: aContext; newLine.  "just class>>selector"	
		aContext := aContext sender].! !

