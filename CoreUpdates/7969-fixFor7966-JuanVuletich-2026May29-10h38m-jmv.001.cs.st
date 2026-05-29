'From Cuis7.7 [latest update: #7968] on 29 May 2026 at 10:38:51 am'!

!DisplayScreen class methodsFor: 'deferred updating' stamp: 'jmv 29/May/2026 10:38:23'!
clearDeferredUpdatingDisplay

	DeferredUpdatingDisplay ifNotNil: [
		DeferredUpdatingDisplay == Display ifFalse: [
			"In case there are still references left."
			DeferredUpdatingDisplay prepareForImageSave ].
		DeferredUpdatingDisplay := nil ].! !

