'From Cuis 4.2 of 25 July 2013 [latest update: #2753] on 4 May 2016 at 5:49:54.205147 pm'!

!DebuggerMethodMap class methodsFor: 'instance creation' stamp: 'pb 5/4/2016 17:40'!
forMethod: aMethod methodNode: methodNode
	"<CompiledMethod>"
	"<MethodNode>"
	"Uncached instance creation method for private use or for tests.
	 Please consider using forMethod: instead."
	^ (aMethod isBlueBookCompiled
		ifTrue: [ DebuggerMethodMapForBlueBookMethods ]
		ifFalse: [ DebuggerMethodMapForClosureCompiledMethods ]) new
		forMethod: aMethod
		methodNode: methodNode.! !


!Form class methodsFor: 'instance creation' stamp: 'pb 5/4/2016 17:43'!
fakeSphereAt: aPoint diameter: diameter color: aColor
	"Create a form which contains a round black dot."
	| bb rect d p |
	bb _ GrafPort toForm: Display.
	bb fillColor: aColor.
	bb combinationRule: Form blend.
	rect _ aPoint extent: diameter.
	bb fillOval: rect.
	p _ (diameter * 2 // 5) asPoint + aPoint.
	#(2 3 4 5 6 ) do: [ :i |
		"simulate facade by circles of gray"
		bb fillColor: (Color white alpha: 0.3).
		d _ i * diameter // 10.
		rect _ Rectangle
			center: p
			extent: d.
		bb fillOval: rect ].
	DisplayScreen screenUpdateRequired: nil.
	"
	Form fakeSphereAt: 50@50 diameter: 30 color:  (Color red alpha: 0.5)
"! !


!ContextPart class methodsFor: 'simulation' stamp: 'pb 5/4/2016 17:39'!
runSimulated: aBlock
	"Simulate the execution of the argument, current. Answer the result it 
	returns."
	^ thisContext sender
		runSimulated: aBlock
		contextAtEachStep: [ :ignored |
			"ContextPart runSimulated: [Pen new defaultNib: 5; go: 100]"
			nil ].! !


!SHTextStylerST80 class methodsFor: 'style table' stamp: 'pb 5/4/2016 17:44'!
initialTextAttributes
	| d element color emphasis attrArray |
	d _ IdentityDictionary new.
	self styleTable do: [ :each |
		element _ each first.
		color _ each
			at: 2
			ifAbsent: nil.
		color _ color ifNotNil: [ Color colorFrom: color ].
		emphasis _ each
			at: 3
			ifAbsent: nil.
		attrArray _ self
			attributeArrayForColor: color
			emphasis: emphasis.
		attrArray notEmpty ifTrue: [
			d
				at: element
				put: attrArray ]].
	^ d.! !

