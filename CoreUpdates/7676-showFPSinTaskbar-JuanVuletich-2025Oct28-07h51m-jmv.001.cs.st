'From Cuis7.5 [latest update: #7673] on 28 October 2025 at 7:55:40 am'!
!classDefinition: #TaskbarMorph category: #'Tools-Taskbar'!
LinearLayoutMorph subclass: #TaskbarMorph
	instanceVariableNames: 'viewBox scale clock showFPS '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Taskbar'!

!TaskbarMorph methodsFor: 'accessing' stamp: 'jmv 10/27/2025 20:36:35'!
clockString
	| ms |
	^String streamContents: [ :strm |
		strm nextPutAll: '  '.
		DateAndTime now printHMSOn: strm.
		showFPS ifTrue: [
			strm nextPutAll: ' FPS: '.
			ms := self world millisecondsSinceLastCycle.
			ms > 1
				ifTrue: [(1000.0 / ms) printOn: strm fractionDigits: 1 ]
				ifFalse: [ strm nextPutAll: '>500' ]].
		strm nextPutAll: ' ' ].! !

!TaskbarMorph methodsFor: 'accessing' stamp: 'jmv 10/28/2025 07:53:39'!
hideFPS
	 showFPS := false.
	clock step; morphExtent: `0@0`.! !

!TaskbarMorph methodsFor: 'accessing' stamp: 'jmv 10/27/2025 20:34:31'!
showFPS
	 showFPS := true.! !


!TaskbarMorph methodsFor: 'initialization' stamp: 'jmv 10/27/2025 20:46:55'!
initialize
	super initialize.
	viewBox := LinearLayoutMorph newRow color: self defaultColor.
	showFPS := false.
	clock := UpdatingLabelMorph 
		contents: '' 
		font: (Preferences at: #windowTitleFont)
		emphasis: AbstractFont  boldCode.
	clock
		target: self;
		getSelector: #clockString;
		stepTime: 1000;
		setBalloonText: [ Date today asString ].
	self
		addMorph: clock
		layoutSpec:  (LayoutSizeSpec morphWidthProportionalHeight: 0.5).
	self
		addMorph: viewBox 
		layoutSpec: (LayoutSizeSpec
			proportionalWidth: 1.0
			proportionalHeight: 1.0 
			offAxisEdgeWeight: #rightOrBottom).
	viewBox separation: self defaultHeight // 8.
	self setBalloonText: [ 
		'About this system:' bold, `String newLineString`,
		Smalltalk systemInformationString ].
	clock
		setProperty: #handlesMouseDown: toValue: true;
		setProperty: #'mouseButton1Down:localPosition:' 
		toValue: [ :evt : pos | | menu taskbarEdge point | 
			self runningWorld ifNotNil: [ :w |
				taskbarEdge := self positionInWorld y.
				menu := (Preferences at: #worldMenu) getMenuIn: w at: w activeHand.	
				menu hide.
				w addMorph: menu. ": Otherwise we do not have a proper extent"
				point := 0 @ (taskbarEdge - menu extentInWorld y).
				menu popUpAt: point allowKeyboard: false ] ]! !

!TaskbarMorph methodsFor: 'menu' stamp: 'jmv 10/28/2025 07:53:30'!
getMenu

	| menu |
	menu := MenuMorph new defaultTarget: self.
	menu
		addLine;
		add: 'Small Height' action: #scaleSmall;
		add: 'Normal Height' action: #scaleNormal;
		add: 'Scale x 2' action: #scaleX2;
		add: 'Scale x 4' action: #scaleX4;
		add: 'Show FPS' action: #showFPS;
		add: 'Hide FPS' action: #hideFPS.
	^menu! !

!classDefinition: #TaskbarMorph category: #'Tools-Taskbar'!
LinearLayoutMorph subclass: #TaskbarMorph
	instanceVariableNames: 'viewBox scale clock showFPS'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Taskbar'!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
UISupervisor ui hideTaskbar; showTaskbar.!

