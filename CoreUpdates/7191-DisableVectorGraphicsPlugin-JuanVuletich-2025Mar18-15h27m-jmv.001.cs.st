'From Cuis7.3 [latest update: #7080] on 18 March 2025 at 3:42:12 pm'!

!WorldMorph methodsFor: 'canvas' stamp: 'jmv 3/18/2025 15:39:34'!
setupNewCanvasAndEngine
	canvas := nil.
	self setupCanvas.
	self whenUIinSafeState: [self whenUIinSafeState: [self restoreDisplay]].! !


!VectorCanvas methodsFor: 'initialization' stamp: 'jmv 3/18/2025 15:40:07'!
setForm: aForm subPixelAntiAliasing: aBoolean
	"nil means use default kind of anti aliasing"
	"
	self runningWorld setupNewCanvasAndEngine
	"
	aForm depth = 32 ifFalse: [ self error: 'VectorCanvas can only work on 32 bit target' ].
	form := aForm.
	engine := VectorEngine onForm: form subPixelAntiAliasing: aBoolean.
	engine canvas: self.
	auxBitBltEngine := BitBltCanvasEngine toForm: form.
	self newClipRect: nil.! !


!VectorCanvas class methodsFor: 'class initialization' stamp: 'jmv 3/18/2025 15:27:23'!
initialize
	UISupervisor whenUIinSafeState: [
		(VectorEngineWithPlugin isPluginAvailable
			ifTrue: [
				MorphicCanvas activeSubclass: HybridCanvas
				"MorphicCanvas activeSubclass: VectorCanvas"
				]
			ifFalse: [
				'VectorEnginePlugin (VM plugin) is not available. HybridCanvas will be activated.' print.
				MorphicCanvas activeSubclass: HybridCanvas ]) ].! !


!VectorEngineWithPlugin class methodsFor: 'instance creation' stamp: 'jmv 3/11/2025 15:59:08'!
isPluginAvailable
	"
	VectorEngineWithPlugin isPluginAvailable
	Check if services provided by VM plugin match this class requirements.
	"
	| pluginApiVersion |
	pluginApiVersion := self pluginApiVersion.
	^ pluginApiVersion = 17! !

VectorCanvas initialize!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
| activeProc guiRoot |
(nil inform: 'After this update, #7191,
we need to restart the User Interface process.
Please do World / Changes... / [Install New Updates].').
[
	ChangeSet installing: '7191-DisableVectorGraphicsPlugin-JuanVuletich-2025Mar18-15h27m-jmv.001.cs.st' do: [].
	cs _ ChangeSet changeSetForBaseSystem.
	(cs name beginsWith: '7191') ifTrue: [
		ChangeSet removeChangeSet: cs ].
	'Installed ChangeSet: 7191-DisableVectorGraphicsPlugin-JuanVuletich-2025Mar18-15h27m-jmv.001.cs.st' print.
	'Please do World / Changes... / [Install New Updates].' print.
	Utilities logsUserChanges: true.
] forkAt: 39.
UISupervisor ui setupNewCanvasAndEngine.
UISupervisor newUIProcess.
Processor activeProcess terminate.
!

