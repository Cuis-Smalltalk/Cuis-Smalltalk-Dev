'From Cuis 6.0 [latest update: #5732] on 28 March 2023 at 7:15:03 pm'!

!InnerTextMorph methodsFor: 'geometry' stamp: 'jmv 3/28/2023 19:13:04'!
extentChanged: oldExtent
	"Our extent changed. Reflow text."
	super extentChanged: oldExtent.
	model ifNotNil: [
		wrapFlag ifTrue: [
			extent x = oldExtent x ifFalse: [
				self resetTextComposition.
				self editor recomputeSelection.	
				self updateFromTextComposition.
				(owner is: #ScrollPane) ifTrue: [
					owner
						updateScrollBarsBounds;
						setScrollDeltas ]]]].! !

