'From Cuis7.9 [latest update: #8069] on 16 July 2026 at 12:37:49 pm'!

!FeatureRequirement commentStamp: 'jmv 16/Jul/2026, 3:35:45 pm (UTC)' prior: 0!
A package has a (possibly empty) set of FeatureRequirement's which specifies acceptable version range and minimum revision.  See class FeatureSpec.

  name -- a symbol naming the feature
  minVersion -- the minimum version required
  maxVersion -- the maximum version required
  minRevision -- the minumum revision of the minVersion (assumed zero if minVersion does not equal maxVersion
  path -- nil or the (cached)  pathName of a file containing the package with this feature

Usually use the convenience methods:
	Feature require: 'Sound'
	Feature require: 'Tests' version: 1.0

Try any of:
  (FeatureRequirement name: #Sound) require.		"Ask for whatever version of Sound is available"
  (FeatureRequirement name: #Sound version: 1) require. 		"Ask for a specific version. Any revision will do"
  (FeatureRequirement name: #Sound version: 1 revision: 0) require.  	"Ask for a specific version.revision"
  (FeatureRequirement name: #Sound minVersion: 1 minRevision: 23 maxVersion: 3) require 		"Specific version.revision, or any greater version, up to max"

This should load packages #Compression and #Sound


  (FeatureRequirement name: #Sound) print
  (FeatureRequirement name: #Sound version: 1) print
  (FeatureRequirement name: #Sound version: 1 revision: 12) print
  (FeatureRequirement name: #Sound minVersion: 1 minRevision: 23 maxVersion: nil) print
  (FeatureRequirement name: #Sound minVersion: 1 minRevision: nil maxVersion: 3) print
  (FeatureRequirement name: #Sound minVersion: 1 minRevision: 23 maxVersion: 3) print!


!NegativePowerError methodsFor: 'exceptionDescription' stamp: 'jmv 18/Jul/2024, 2:04:13 pm (UTC)'!
defaultAction

	"Disable this preference to have Float nan answer (if Float receiver or argument) or Error message"
	| answer |
	(Preferences at: #askToInstallComplexPackage) ifTrue: [
		answer := PopUpMenu
			withCaption:
'Square (or even) Root of a negative Number:
Complex number support is not loaded
Would you like me to load it for you now?'
			chooseFrom: #(
				'Load Complex package'
				'Do not load Complex package'
				'Do not load Complex package and don''t ask again').
		answer = 1 ifTrue: [
			Feature requireInformFaiure: #Complex.
			Smalltalk at: #Complex ifPresent: [ :cplx |
				^ (cplx basicReal: receiver imaginary: 0) perform: selector withArguments: arguments ]].
		answer = 3 ifTrue: [
			(Preferences at: #askToInstallComplexPackage put: false)]].
	^ super defaultAction! !


!Morph methodsFor: 'events' stamp: 'KenD 13/May/2015, 2:32:00 pm (UTC)'!
click: aMouseButtonEvent localPosition: localEventPosition
	"Handle a single-click event. This message is only sent to clients that request it by sending one of the #waitForClicksOrDrag:... messages to the initiating hand in their mouseDown: method. This default implementation does nothing."
	
	self 
		valueOfProperty: #click:localPosition:
		ifPresentDo: [ :handler | handler value:  aMouseButtonEvent value: localEventPosition ]! !

!Morph methodsFor: 'events' stamp: 'KenD 13/May/2015, 2:32:00 pm (UTC)'!
doubleClick: aMouseButtonEvent localPosition: localEventPosition
	"Handle a double-click event. This message is only sent to clients that request it by sending one of the #waitForClicksOrDrag:... messages to the initiating hand in their mouseDown: method. This default implementation does nothing."
	
	"Allow instances to dynamically use properties for handling common events."
	self 
		valueOfProperty: #doubleClick:localPosition:
		ifPresentDo: [ :handler | handler value:  aMouseButtonEvent value: localEventPosition ]! !

!Morph methodsFor: 'events' stamp: 'KenD 13/May/2015, 2:33:00 pm (UTC)'!
mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition
	"Handle a mouse button 1 up event.
	This message will only be sent to Morphs that answer true to #handlesMouseDown:"
	
	"Allow instances to dynamically use properties for handling common events."
	self
		valueOfProperty: #mouseButton1Up:localPosition:
		ifPresentDo: [ :handler | handler value: aMouseButtonEvent value: localEventPosition ]! !

!Morph methodsFor: 'events' stamp: 'JEC 15/May/2025, 1:09:39 pm (UTC)'!
mouseButton2Down: aMouseButtonEvent localPosition: localEventPosition
	"Handle a mouse button 2 down event.
	This message will only be sent to Morphs that answer true to #handlesMouseDown:"

	self
		valueOfProperty: #mouseButton2Down:localPosition:
		ifPresentDo: [ :handler |
			handler value: aMouseButtonEvent value: localEventPosition.
			^self ].

	self mouseButton2Activity.! !

!Morph methodsFor: 'events' stamp: 'JEC 15/May/2025, 1:09:53 pm (UTC)'!
mouseButton2Up: aMouseButtonEvent localPosition: localEventPosition
	"Handle a mouse button 2 up event.
	This message will only be sent to Morphs that answer true to #handlesMouseDown:"

	self
		valueOfProperty: #mouseButton2Up:localPosition:
		ifPresentDo: [ :handler | handler value: aMouseButtonEvent value: localEventPosition ]! !

!Morph methodsFor: 'events' stamp: 'KenD 13/May/2015, 2:33:00 pm (UTC)'!
mouseStillDown
	"Called from the stepping mechanism for morphs wanting continuously repeated 'yes the mouse is still down, yes it is still down, yes it has not changed yet, no the mouse is still not up, yes the button is down' etc messages"
	
	self "Allow instances to use this"
		valueOfProperty: #mouseStillDown
		ifPresentDo: [ :handler | handler value ]! !

!Morph methodsFor: 'events' stamp: 'KenD 13/May/2015, 2:34:00 pm (UTC)'!
windowEvent: anEvent
	"Host window event"
	
	"Allow instances to dynamically use properties for handling common events."
	self
		valueOfProperty: #windowEvent:
		ifPresentDo: [ :handler | handler value: anEvent ]! !

!Morph methodsFor: 'event handling testing' stamp: 'HAW 24/Dec/2018, 10:13:50 am (UTC)'!
allowsFilesDrop
	"Answer whether we accept dropping files. By default answer false."

	"Use a property test to allow individual instances to specify this."
	^ self hasProperty: #allowsFilesDrop! !

!Morph methodsFor: 'event handling testing' stamp: 'KenD 13/May/2015, 2:34:00 pm (UTC)'!
allowsSubmorphDrag
	"Answer whether our morphs can just be grabbed with the hand, instead of requiring the use of the halo. By default answer false."

	"Use a property test to allow individual instances to specify this."
	^ self hasProperty: #allowsSubmorphDrag! !

!Morph methodsFor: 'event handling testing' stamp: 'len 13/Nov/2025, 10:52:46 pm (UTC)'!
handlesDragOver: aMouseEvent
	"Do I want to receive dragEnter: and dragLeave: when the hand is dragging something?  The default response is false." 
	"Use a property test to allow individual instances to specify this."
	^ self hasProperty: #handlesDragOver:! !

!Morph methodsFor: 'event handling testing' stamp: 'KenD 13/May/2015, 2:34:00 pm (UTC)'!
handlesMouseDown: aMouseButtonEvent
	"Do I want to receive mouseButton messages ?
	- #mouseButton1Down:localPosition:
	- #mouseButton1Up:localPosition:
	- #mouseButton2Down:localPosition:
	- #mouseButton2Up:localPosition:
	- #mouseButton3Down:localPosition:
	- #mouseButton3Up:localPosition:
	- #mouseMove:localPosition:
	- #mouseButton2Activity
	
	NOTE: The default response is false. Subclasses that implement these messages directly should override this one to return true.
	
	Implementors could query the argument, and only answer true for (for example) button 2 up only."

	"Use a property test to allow individual instances to dynamically specify this."
	^ self hasProperty: #handlesMouseDown:! !

!Morph methodsFor: 'event handling testing' stamp: 'jmv 3/Sep/2025, 11:40:16 pm (UTC)'!
handlesMouseOver: aMouseEvent
	"Do I want to receive mouseEnter: and mouseLeave: when the button is up and the hand is empty?  The default response is false." 

	"Use a property test to allow individual instances to specify this."
	^ self hasProperty: #handlesMouseOver:! !

!Morph methodsFor: 'event handling testing' stamp: 'pb 22/Oct/2017, 5:53:48 am (UTC)'!
handlesMouseScroll: aMouseScrollEvent
	^ self hasProperty: #handlesMouseScroll:! !

!Morph methodsFor: 'event handling testing' stamp: 'KenD 13/May/2015, 2:34:00 pm (UTC)'!
handlesMouseStillDown: evt
	"Return true if the receiver wants to get repeated #mouseStillDown: messages between #mouseDown: and #mouseUp"

	"Use a property test to allow individual instances to specify this."
	^ self hasProperty: #handlesMouseStillDown:! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 16/Jul/2026, 2:44:58 pm (UTC)'!
processKeyDown: aKeyboardEvent
	"A KeyboardEvent was delivered to us.
	If appropriate, handle it and mark it as #wasHandled: true."

	self keyDown: aKeyboardEvent.
	self
		valueOfProperty: #keyDown:
		ifPresentDo: [ :handler |
			handler value: aKeyboardEvent.
			aKeyboardEvent wasHandled: true ].! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 16/Jul/2026, 2:45:51 pm (UTC)'!
processKeyUp: aKeyboardEvent
	"A KeyboardEvent was delivered to us.
	If appropriate, handle it and mark it as #wasHandled: true."

	self keyUp: aKeyboardEvent.
	self
		valueOfProperty: #keyUp:
		ifPresentDo: [ :handler |
			handler value: aKeyboardEvent.
			aKeyboardEvent wasHandled: true ].! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 23/Sep/2025, 1:08:59 pm (UTC)'!
processKeystroke: aKeyboardEvent
	"A KeyboardEvent was delivered to us.
	If appropriate, handle it and mark it as #wasHandled: true."

	self keyStroke: aKeyboardEvent.
	self
		valueOfProperty: #keyStroke:
		ifPresentDo: [ :handler |
			handler value: aKeyboardEvent.
			aKeyboardEvent wasHandled: true ].! !


!LayoutMorph class methodsFor: 'LinearLayout examples' stamp: 'jmv 16/Jul/2026, 3:36:58 pm (UTC)'!
iconsExample
	"
	LayoutMorph iconsExample
	"
	| lm ilm counter |
	lm := LinearLayoutMorph newColumn.
	ilm := LinearLayoutMorph newRow.
	counter := 1.
	Theme methodsDo: [ :method | | sel | 
		(sel := method selector) numArgs = 0 ifTrue: [
			(sel endsWith: 'Icon') ifTrue: [ | mayBeForm |
				mayBeForm := Theme current perform: sel.
				(mayBeForm isKindOf: Form) ifTrue: [ | imageMorph |
					counter \\ 10 = 0 ifTrue: [
						lm addMorph: ilm.
						ilm := LinearLayoutMorph newRow ].
					counter := counter + 1.
					ilm addMorph: (imageMorph := ImageMorph newWith: (mayBeForm magnifyBy: 2)).
					imageMorph 
						setBalloonText: 'Copy ''', sel storeString bold, ''' to the Clipboard';
						setProperty: #handlesMouseDown: toValue: true;
						setProperty: #mouseButton1Down:localPosition:  toValue: [ :event :pos |
							Clipboard storeObject: sel storeString ] ] ] ] ].
	lm 
		setProperty: #taskbarButtonIcon toValue: Theme current imageIcon;
		setBalloonText: 'Drag me here!!'.
	lm openInWorld
! !


!TaskbarMorph methodsFor: 'initialization' stamp: 'jmv 16/Jul/2026, 3:33:40 pm (UTC)'!
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
		setProperty: #mouseButton1Down:localPosition:
		toValue: [ :evt : pos | | menu taskbarEdge point | 
			self runningWorld ifNotNil: [ :w |
				taskbarEdge := self positionInWorld y.
				menu := (Preferences at: #worldMenu) getMenuIn: w at: w activeHand.	
				menu hide.
				w addMorph: menu. ": Otherwise we do not have a proper extent"
				point := 0 @ (taskbarEdge - menu extentInWorld y).
				menu popUpAt: point allowKeyboard: false ] ].! !


!TheWorldMenu methodsFor: 'menu' stamp: 'jmv 16/Jul/2026, 3:34:03 pm (UTC)'!
openPackageDownloader
	"In PackageDownloader.pck"
	Smalltalk at: #PackageDownloaderWindow ifAbsent: [ 
		(PopUpMenu 
		confirm: 'The Package Downloader is not loaded', String newLineString, 'Would you like me to load it for you now?')
		ifTrue: [Feature requireInformFaiure: #PackageDownloader ] 
	].
	Smalltalk at: #PackageDownloaderWindow ifPresent: [ :cls | ^cls open ].
	self inform: 'Please install optional package PackageDownloader.pck.st', String newLineString, 'Feature require: #''PackageDownloader'' '.! !

!TheWorldMenu methodsFor: 'menu' stamp: 'jmv 16/Jul/2026, 3:34:11 pm (UTC)'!
openPackageInstaller
	"In PackageInstaller.pck"
	Smalltalk at: #PackageInstallerWindow ifAbsent: [ 
		(PopUpMenu 
		confirm: 'The Package Installer is not loaded', String newLineString, 'Would you like me to load it for you now?')
		ifTrue: [Feature requireInformFaiure: #PackageInstaller ] 
	].
	Smalltalk at: #PackageInstallerWindow ifPresent: [ :cls | ^cls open ].
	self inform: 'Please install optional package PackageInstaller.pck.st', String newLineString, 'Feature require: #''PackageInstaller'' '.! !

!TheWorldMenu methodsFor: 'menu' stamp: 'jmv 16/Jul/2026, 3:34:20 pm (UTC)'!
openTerseGuide
	"In TerseGuide.pck"
	Smalltalk at: #TerseGuideWindow ifAbsent: [ 
		(PopUpMenu 
		confirm: 'The Terse Guide is not loaded', String newLineString, 'Would you like me to load it for you now?')
		ifTrue: [Feature requireInformFaiure: #TerseGuide] 
	].
	Smalltalk at: #TerseGuideWindow ifPresent: [ :cls | ^cls openTerseGuide ].
	self inform: 'Please install optional package TerseGuide.pck.st', String newLineString, 'Feature require: #''TerseGuide'' '.! !

