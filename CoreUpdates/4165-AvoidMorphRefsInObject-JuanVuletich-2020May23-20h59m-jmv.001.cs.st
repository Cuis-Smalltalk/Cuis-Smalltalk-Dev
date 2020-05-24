'From Cuis 5.0 [latest update: #4163] on 23 May 2020 at 9:00:17 pm'!

!Object methodsFor: 'user interface' stamp: 'jmv 5/23/2020 21:00:08'!
request: queryString initialAnswer: defaultAnswer orCancel: cancelBlock
	"This is deprecated because using it requires blocking the user interface until the user provides a response. Please use the variants that are not modal."
	^ UISupervisor ui request: queryString initialAnswer: defaultAnswer orCancel: cancelBlock! !

!Object methodsFor: 'user interface' stamp: 'jmv 5/23/2020 21:00:11'!
request: queryString initialAnswer: defaultAnswer verifying: validationBlock do: acceptBlock orCancel: cancelBlock
	^ UISupervisor ui request: queryString initialAnswer: defaultAnswer verifying: validationBlock do: acceptBlock orCancel: cancelBlock! !


!PasteUpMorph methodsFor: 'ui services' stamp: 'jmv 5/23/2020 21:00:01'!
request: queryString initialAnswer: defaultAnswer orCancel: cancelBlock
	"This is deprecated because using it requires blocking the user interface until the user provides a response. Please use the variants that are not modal."
	^ StringRequestMorph request: queryString initialAnswer: defaultAnswer orCancel: cancelBlock! !

!PasteUpMorph methodsFor: 'ui services' stamp: 'jmv 5/23/2020 21:00:05'!
request: queryString initialAnswer: defaultAnswer verifying: validationBlock do: acceptBlock orCancel: cancelBlock
	^ StringRequestMorph request: queryString initialAnswer: defaultAnswer verifying: validationBlock do: acceptBlock orCancel: cancelBlock! !

