'From Cuis7.3 [latest update: #7100] on 22 April 2025 at 4:38:06 pm'!

!TextProvider methodsFor: 'testing' stamp: 'HAW 4/22/2025 16:36:44'!
isEditingExistingMethod

	^self isEditingMethod! !

!TextProvider methodsFor: 'testing' stamp: 'HAW 4/22/2025 16:36:44'!
isEditingMethod

	^false! !


!ObjectExplorer methodsFor: 'testing' stamp: 'HAW 4/22/2025 16:37:36'!
isEditingExistingMethod

	^self isEditingMethod! !

!ObjectExplorer methodsFor: 'testing' stamp: 'HAW 4/22/2025 16:37:31'!
isEditingMethod

	^false! !

!methodRemoval: CodeProvider #isEditingMethod stamp: 'HAW 4/22/2025 16:36:44'!
CodeProvider removeSelector: #isEditingMethod!
!methodRemoval: CodeProvider #isEditingExistingMethod stamp: 'HAW 4/22/2025 16:36:44'!
CodeProvider removeSelector: #isEditingExistingMethod!
