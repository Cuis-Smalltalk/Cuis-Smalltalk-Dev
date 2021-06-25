'From Cuis 5.0 [latest update: #4647] on 25 June 2021 at 12:34:07 pm'!

!Encoder methodsFor: 'private' stamp: 'MM 6/25/2021 11:55:34'!
warnAboutShadowed: name

	| msg transcriptMsg |
	
	msg _ 'There already exists a variable named ', name, ' '.
	requestor addWarning: msg.
	
	transcriptMsg _ msg, ' (', class className, '>>', selector printString,')'. 
	Transcript newLine; show: transcriptMsg.
	
	InMidstOfFileinNotification signal
		ifFalse: [
			requestor interactive
				ifTrue: [
					self notify: msg]
				ifFalse: [
					(RecompilationFailure
						class: class
						messageText: msg, String newLineString, 'Please use a different name') signal ]]! !

