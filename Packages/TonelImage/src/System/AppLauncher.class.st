Class {
	#name : 'AppLauncher',
	#superclass : 'Object',
	#classVars : [
		'AppGlobalName',
		'AppSelector'
	],
	#category : 'System-Support',
	#package : 'System',
	#tag : 'Support'
}

{ #category : 'accessing' }
AppLauncher class >> appGlobalName [

	^ AppGlobalName
]

{ #category : 'accessing' }
AppLauncher class >> appGlobalName: appGlobalName appSelector: appSelector [
	"For starting end user code (i.e. applications) on startup"

	AppGlobalName := appGlobalName.
	AppSelector := appSelector
]

{ #category : 'accessing' }
AppLauncher class >> appSelector [

	^ AppSelector
]

{ #category : 'system startup' }
AppLauncher class >> launchApp [

	Smalltalk at: AppGlobalName ifPresent: [ :cls |
		cls perform: AppSelector withPossiblyWrongSizedArguments: Smalltalk startUpArguments ]
]
