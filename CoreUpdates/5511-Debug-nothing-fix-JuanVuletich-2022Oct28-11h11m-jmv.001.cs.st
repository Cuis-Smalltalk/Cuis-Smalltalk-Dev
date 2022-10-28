'From Cuis 6.0 [latest update: #5535] on 28 October 2022 at 11:13:45 am'!

!SmalltalkEditor methodsFor: 'do-its' stamp: 'jmv 10/28/2022 11:08:31'!
debugIt

	self lineSelectAndEmptyCheck: [^self].
	self 
		afterCompiling: self selection
		do: [ :compiler :method :receiver :context |
			(method notNil and: [method isQuick not]) ifTrue: [
				self debug: method receiver: receiver in: context ]]
		ifFail: [].! !

