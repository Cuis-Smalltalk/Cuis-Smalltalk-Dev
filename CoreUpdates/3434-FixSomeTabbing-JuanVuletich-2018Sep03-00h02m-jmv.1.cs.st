'From Cuis 5.0 of 7 November 2016 [latest update: #3430] on 3 September 2018 at 12:04:11 am'!

!Number methodsFor: 'printing' stamp: 'jmv 9/3/2018 00:03:09'!
withBinaryUnitPrefixAndValue: aBlock
	"
	As in https://en.wikipedia.org/wiki/Binary_prefix
	{ 0.123456. 1.23456. 12.3456. 123.456. 1234.56. 12345.6. 123456. 1234560. 12345600 } do: [ :n | n withBinaryUnitPrefixAndValue: [ :value  :unitPrefixSymbol :unitPrefixName | {value printString, ' ', unitPrefixSymbol. unitPrefixName} print]]
	"
	| prefixIndex factor nameAndSymbol |
	prefixIndex _ self log floor // 3.
	prefixIndex _ prefixIndex min: 8 max: 0.
	factor _ 1024 raisedToInteger: prefixIndex.
	nameAndSymbol _ {
		{''. 		''}.
		{'kibi'.	'Ki'}.
		{'mebi'.	'Mi'}.
		{'gibi'.	'Gi'}.
		{'tebi'.	'Ti'}.
		{'pebi'.	'Pi'}.
		{'exbi'.	'Ei'}.
		{'zebi'.	'Zi'}.
		{'yobi'.	'Yi'}
	} at: prefixIndex+1.
	aBlock value: (self / factor) asIntegerOrFloat value: nameAndSymbol second value: nameAndSymbol first! !

!Number methodsFor: 'printing' stamp: 'jmv 9/3/2018 00:03:34'!
withDecimalUnitPrefixAndValue: aBlock
	"
	As in https://en.wikipedia.org/wiki/Metric_prefix
	{ 0.00000123456. 0.0000123456. 0.000123456. 0.00123456. 0.0123456. 0.123456. 1.23456. 12.3456. 123.456. 1234.56. 12345.6. 123456. 1234560. 12345600 } do: [ :n | n withDecimalUnitPrefixAndValue: [ :value  :unitPrefixSymbol :unitPrefixName | {value printString, ' ', unitPrefixSymbol. unitPrefixName} print]]
	"
	| prefixIndex factor nameAndSymbol |
	prefixIndex _ self log floor // 3.
	prefixIndex _ prefixIndex min: 6 max: -6.
	factor _ 1000 raisedToInteger: prefixIndex.
	nameAndSymbol _ {
		{'atto'.		'a'}.
		{'femto'.	'f'}.
		{'pico'.		'p'}.
		{'nano'.		'n'}.
		{'micro'.	'µ'}.
		{'milli'.	'm'}.
		{''. 			''}.
		{'kilo'.		'k'}.
		{'mega'.		'M'}.
		{'giga'.		'G'}.
		{'tera'.		'T'}.
		{'peta'.		'P'}.
		{'exa'.		'E'}
	} at: prefixIndex+7.
	aBlock value: self asFloat / factor value: nameAndSymbol second value: nameAndSymbol first! !


!SmalltalkEditor class methodsFor: 'menu' stamp: 'jmv 9/3/2018 00:03:51'!
smalltalkEditorMenuOptions
	
	^`{
			{
				#itemGroup 		-> 		10.
				#itemOrder 		-> 		10.
				#label 			-> 		'Help...'.
				#selector 		-> 		#openHelp.
				#icon 			-> 		#helpIcon
			} asDictionary.
			{
				#itemGroup 		-> 		20.
				#itemOrder 		-> 		10.
				#label 			-> 		'Do it (d)'.
				#selector 		-> 		#doIt.
				#icon 			-> 		#doItIcon
			} asDictionary.
			{
				#itemGroup 		-> 		20.
				#itemOrder 		-> 		20.
				#label 			-> 		'Print it (p)'.
				#selector 		-> 		#printIt.
				#icon 			-> 		#printIcon
			} asDictionary.
			{
				#itemGroup 		-> 		20.
				#itemOrder 		-> 		30.
				#label 			-> 		'Inspect it (i)'.
				#selector 		-> 		#inspectIt.
				#icon 			-> 		#inspectIcon
			} asDictionary.
			{
				#itemGroup 		-> 		20.
				#itemOrder 		-> 		40.
				#label 			-> 		'Explore it (I)'.
				#selector 		-> 		#exploreIt.
				#icon 			-> 		#exploreIcon
			} asDictionary.
			{
				#itemGroup 		-> 		20.
				#itemOrder 		-> 		50.
				#label 			-> 		'Debug it (D)'.
				#selector 		-> 		#debugIt.
				#icon 			-> 		#debugIcon
			} asDictionary.
			{
				#itemGroup 		-> 		20.
				#itemOrder 		-> 		60.
				#label 			-> 		'Profile it'.
				#selector 		-> 		#profileIt.
				#icon 			-> 		#clockIcon
			} asDictionary.
			{
				#itemGroup 		-> 		30.
				#itemOrder 		-> 		10.
				#label 			-> 		'Explain'.
				#selector 		-> 		#explain.
				#icon 			-> 		#helpIcon
			} asDictionary.
			{
				#itemGroup 		-> 		30.
				#itemOrder 		-> 		20.
				#label 			-> 		'Browse it (b)'.
				#selector 		-> 		#browseIt.
				#icon 			-> 		#editFindReplaceIcon
			} asDictionary.
			{
				#itemGroup 		-> 		30.
				#itemOrder 		-> 		30.
				#label 			-> 		'Senders of it (n)'.
				#selector 		-> 		#sendersOfIt.
				#icon 			-> 		#mailForwardIcon
			} asDictionary.
			{
				#itemGroup 		-> 		30.
				#itemOrder 		-> 		40.
				#label 			-> 		'Implementors of it (m)'.
				#selector 		-> 		#implementorsOfIt.
				#icon 			-> 		#developmentIcon
			} asDictionary.
			{
				#itemGroup 		-> 		30.
				#itemOrder 		-> 		50.
				#label 			-> 		'References to it (N)'.
				#selector 		-> 		#referencesToIt.
				#icon 			-> 		#addressBookIcon
			} asDictionary.
			{
				#itemGroup 		-> 		40.
				#itemOrder 		-> 		10.
				#label 			-> 		'Method Strings with it (E)'.
				#selector 		-> 		#methodStringsContainingit.
				#icon 			-> 		#genericTextIcon
			} asDictionary.
			{
				#itemGroup 		-> 		40.
				#itemOrder 		-> 		20.
				#label 			-> 		'Method Source with it'.
				#selector 		-> 		#methodSourceContainingIt.
				#icon 			-> 		#scriptIcon
			} asDictionary.
			{
				#itemGroup 		-> 		40.
				#itemOrder 		-> 		30.
				#label 			-> 		'Class Comments with it'.
				#selector 		-> 		#classCommentsContainingIt.
				#icon 			-> 		#chatIcon
			} asDictionary.
			{
				#itemGroup 		-> 		50.
				#itemOrder 		-> 		10.
				#label 			-> 		'Accept (s)'.
				#selector 		-> 		#acceptContents.
				#icon 			-> 		#acceptIcon
			} asDictionary.
			{
				#itemGroup 		-> 		50.
				#itemOrder 		-> 		30.
				#label 			-> 		'Accept & Run Test in Class (t)'.
				#selector 		-> 		#acceptAndTest.
				#icon 			-> 		#weatherFewCloudsIcon
			} asDictionary.
			{
				#itemGroup 		-> 		50.
				#itemOrder 		-> 		40.
				#label 			-> 		'Accept & Run Test in Category (y)'.
				#selector 		-> 		#acceptAndTestAll.
				#icon 			-> 		#weatherFewCloudsIcon
			} asDictionary.
			{
				#itemGroup 		-> 		50.
				#itemOrder 		-> 		50.
				#label 			-> 		'Accept & Debug Test (r)'.
				#selector 		-> 		#acceptAndDebugTest.
				#icon 			-> 		#weatherFewCloudsIcon
			} asDictionary.
			{
				#itemGroup 		-> 		60.
				#itemOrder 		-> 		10.
				#label 			-> 		'More...'.
				#selector 		-> 		#openMenu2.
				#icon 			-> 		#listAddIcon
			} asDictionary.
		}`! !


!Transcript class methodsFor: 'preferred protocol' stamp: 'jmv 9/3/2018 00:04:05'!
clear

	accessSemaphore critical: [
		"Having at least one entry simplifies handling of the entries circular collection"
		firstIndex _ 1.
		lastIndex _ 1.
		entries at: 1 put: 'Transcript'.
		unfinishedEntry reset.
		lastDisplayPosition _ 0.
		
		logToFile ifTrue: [
			self filename asFileEntry forceWriteStreamDo: [ :stream |
				stream nextPutAll: 'Transcript log started: '.
				DateAndTime now printOn: stream.
				stream
					newLine;
					nextPutAll: '------------------------------------------------------------------------';
					newLine ]]].
	self display! !

!Transcript class methodsFor: 'preferred protocol' stamp: 'jmv 9/3/2018 00:04:08'!
clearInternal

	accessSemaphore critical: [
		"Having at least one entry simplifies handling of the entries circular collection"
		firstIndex _ 1.
		lastIndex _ 1.
		entries at: 1 put: 'Transcript'.
		unfinishedEntry reset.
		lastDisplayPosition _ 0 ].
	self display! !


!PreDebugWindow methodsFor: 'GUI building' stamp: 'jmv 9/3/2018 00:03:40'!
preDebugButtonSpec

	^{
		{'Proceed'.		#proceed. 	'continue execution' }.
		{'Abandon'.		#abandon. 	'abandon this execution by closing this window' }.
		{'Debug'.		#debug.		'bring up a debugger' }
	}! !

