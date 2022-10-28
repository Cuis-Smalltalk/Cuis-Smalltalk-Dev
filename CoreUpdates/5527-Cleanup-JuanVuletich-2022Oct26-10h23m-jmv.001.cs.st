'From Cuis 6.0 [latest update: #5523] on 26 October 2022 at 10:24:34 am'!

!SystemDictionary methodsFor: 'query' stamp: 'jmv 10/1/2022 12:02:12'!
classesWithExtensionCategories
	"For a base image, there should be none.
	Smalltalk classesWithExtensionCategories
	"

	Object withAllSubclasses do: [ :cls | cls organization categories do: [ :cat | cat first = $* ifTrue: [cls print ]]]! !


!PluggableButtonMorph reorganize!
('accessing' actWhen: action: actionSelector adoptWidgetsColor: font font: icon: iconDrawSelector iconDrawSelector: iconName iconName: label label: label:font: magnifiedIcon: mouseOverColor: performAction roundButtonStyle:)
('drawing' draw3DLookOn: drawEmbossedLabelOn: drawOn: drawRegularLabelOn: drawRoundLookOn: enableColorWith: fontToUse)
('events' mouseButton1Down:localPosition: mouseButton1Up:localPosition: mouseButton2Up:localPosition: mouseEnter: mouseLeave: mouseStillDown)
('event handling testing' handlesMouseDown: handlesMouseOver: handlesMouseStillDown:)
('event handling' mouseStillDownStepRate)
('initialization' actionArgument: defaultBorderColor defaultBorderWidth enableSelector: initialize model:stateGetter:action:label: model:stateGetter:stateArgument:action:actionArgument:label: secondaryActionSelector: stateArgument:)
('updating' update:)
('private' getEnabled getState magnifiedIcon)
('testing' is: isDisabled isEnabled isPressed isRoundButton mouseIsOver)
('geometry' extentChanged: minimumExtent)
('enabling' disable enable)
!


!CodePackageFile reorganize!
('services' install)
('accessing' classesToRemove description featureSpec methodsToRemove packageDescription packageName provides requires summary)
('change record types' doIt: possibleDescription: provides: requires:)
('initialization' buildFileStream:packageName:fullName: initialize)
!

