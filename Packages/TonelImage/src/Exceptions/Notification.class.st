"
A Notification is an indication that something interesting has occurred.  If it is not handled, it will pass by without effect.
"
Class {
	#name : 'Notification',
	#superclass : 'Exception',
	#category : 'Exceptions-Kernel',
	#package : 'Exceptions',
	#tag : 'Kernel'
}

{ #category : 'debugger updating - signaling' }
Notification class >> signalToUpdateDebuggerOn: unhandledErrorSignalerContext dueTo: anError [

	self new 
		tag: {unhandledErrorSignalerContext. anError};
		signal.
]

{ #category : 'exceptionDescription' }
Notification >> defaultAction [
	"No action is taken. The value nil is returned as the value of the message that signaled the exception."


	^nil
]

{ #category : 'exceptionDescription' }
Notification >> isResumable [
	"Answer true. Notification exceptions by default are specified to be resumable."

	^true
]

{ #category : 'debugger updating' }
Notification >> isToUpdateDebuggerOn: aContext [

	^tag isArray
		and: [tag size = 2
		and: [(tag first == aContext or: [tag first hasSender: aContext])]]
]

{ #category : 'debugger updating' }
Notification >> withNewDebuggerLabelOn: aContext do: labelBlock ifNone: noneBlok [

	^(self isToUpdateDebuggerOn: aContext)
		ifTrue: [ labelBlock value: tag second description ]
		ifFalse: noneBlok 
]
