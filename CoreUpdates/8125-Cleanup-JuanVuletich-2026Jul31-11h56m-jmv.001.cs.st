'From Cuis7.9 [latest update: #8120] on 31 July 2026 at 11:58:38 am'!

!ClassDescription methodsFor: 'compiling' stamp: 'jmv 30/Jul/2026, 10:03:16 pm (UTC)'!
compile: text classified: category withStamp: changeStamp notifying: requestor

	^ self compile: text classified: category withStamp: changeStamp methodProperties: nil notifying: requestor logSource: self acceptsLoggingOfCompilation! !

!ClassDescription methodsFor: 'compiling' stamp: 'jmv 30/Jul/2026, 10:03:20 pm (UTC)'!
compileSilently: code classified: category notifying: requestor
	"Compile the code and classify the resulting method in the given category, leaving no trail in the system log, nor in any change set, nor in the 'recent submissions' list. This should only be used when you know for sure that the compilation will succeed."

	^ SystemChangeNotifier uniqueInstance 
		doSilently: [self compile: code classified: category withStamp: nil methodProperties: nil notifying: requestor logSource: false].! !

