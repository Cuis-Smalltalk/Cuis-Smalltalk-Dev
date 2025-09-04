'From Cuis7.5 [latest update: #7507] on 4 September 2025 at 1:17:29 pm'!

!MenuItemMorph methodsFor: 'events-processing' stamp: 'jmv 9/4/2025 13:17:16'!
processMouseEnter: aMouseEnterEvent localPosition: localEventPosition
	"A MorphicEvent was delivered to us.
	If appropriate, handle it and mark it as #wasHandled: true."
	
	self wantsBalloon ifTrue: [
		aMouseEnterEvent hand triggerBalloonFor: self after: self balloonHelpDelayTime].
	(self handlesMouseOver: aMouseEnterEvent) ifTrue:[
		aMouseEnterEvent wasHandled: true.
		self mouseEnter: aMouseEnterEvent ]! !

!MenuItemMorph methodsFor: 'events-processing' stamp: 'jmv 9/4/2025 13:17:21'!
processMouseLeave: aMouseLeaveEvent localPosition: localEventPosition
	"A MorphicEvent was delivered to us.
	If appropriate, handle it and mark it as #wasHandled: true."
	
	aMouseLeaveEvent hand removePendingBalloonFor: self.
	(self handlesMouseOver: aMouseLeaveEvent) ifTrue: [
		aMouseLeaveEvent wasHandled: true.
		self mouseLeave: aMouseLeaveEvent ]! !

