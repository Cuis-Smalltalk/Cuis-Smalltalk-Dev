'From Cuis 5.0 of 7 November 2016 [latest update: #3815] on 26 June 2019 at 2:34:09 pm'!

!methodRemoval: ProgressInitiationException #aPoint!
ProgressInitiationException removeSelector: #aPoint!
!methodRemoval: ProgressInitiationException #currentVal!
ProgressInitiationException removeSelector: #currentVal!
!methodRemoval: ProgressInitiationException #currentVal:!
ProgressInitiationException removeSelector: #currentVal:!
!methodRemoval: ProgressInitiationException #maxVal!
ProgressInitiationException removeSelector: #maxVal!
!methodRemoval: ProgressInitiationException #minVal!
ProgressInitiationException removeSelector: #minVal!
!methodRemoval: ProgressInitiationException #progressTitle!
ProgressInitiationException removeSelector: #progressTitle!
!methodRemoval: ProgressInitiationException #title:at:from:to:workBlock:!
ProgressInitiationException removeSelector: #title:at:from:to:workBlock:!
!methodRemoval: ProgressInitiationException #workBlock!
ProgressInitiationException removeSelector: #workBlock!

!ProgressInitiationException reorganize!
('exceptionDescription' defaultAction display:at:from:to:during: isResumable sendNotificationsTo:)
!

