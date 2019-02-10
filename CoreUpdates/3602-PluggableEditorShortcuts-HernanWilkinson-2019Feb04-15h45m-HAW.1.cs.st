'From Cuis 5.0 of 7 November 2016 [latest update: #3597] on 4 February 2019 at 4:55:07 pm'!

!Editor class methodsFor: 'class initialization' stamp: 'HAW 2/4/2019 16:54:10'!
initializeCmdShortcutsUsing: anInitializationMessage

	| dynamicInitializationMessage |
	
	self putIntoCmdShortcuts: (self perform: anInitializationMessage).

	dynamicInitializationMessage := (self name asString uncapitalized, anInitializationMessage asString capitalized) asSymbol.
	(Smalltalk allClassesImplementing: dynamicInitializationMessage) do: [ :aClass | 
		self putIntoCmdShortcuts: (aClass soleInstance perform: dynamicInitializationMessage) ].


! !

!Editor class methodsFor: 'class initialization' stamp: 'HAW 2/4/2019 16:53:40'!
putIntoCmdShortcuts: shortcutsSpec

	shortcutsSpec do: [ :ary | cmdShortcuts at: ary first numericValue + 1 put: ary second ].! !


!Editor class methodsFor: 'class initialization' stamp: 'HAW 2/4/2019 16:51:32'!
initializeCmdShortcuts
	"Initialize the (unshifted) command-key (or alt-key if not on Mac) shortcut table.
	If you want to add a new shortcut for an specific editor, for example SmalltalkEditor, you should
	define the message #smalltalkEditorCmdShortcutsSpec in a class of your category and it will
	be dynamically send"

	"NOTE: if you don't know what your keyboard generates, use Sensor test"

	"
	Editor initialize
	"

	cmdShortcuts _ Array new: 256 withAll: #noop:.

	self initializeCmdShortcutsUsing: #basicCmdShortcutsSpec.
	self initializeCmdShortcutsUsing: #cmdShortcutsSpec.
! !

