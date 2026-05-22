Class {
	#name : 'JpegTest',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-Graphics-Files',
	#package : 'BaseImageTests',
	#tag : 'Graphics-Files'
}

{ #category : 'test examples' }
JpegTest class >> lenaColor64 [
	"
	('LenaColor64.bmp' asFileEntry readStreamDo: [ :strm | strm binary. Base64MimeConverter mimeEncode: strm ]) upToEnd
	JpegTest lenaColor64 display
	"
	^Form fromBinaryStream:
		'Qk02MAAAAAAAADYAAAAoAAAAQAAAAEAAAAABABgAAAAAAAAwAADwCgAA8AoAAAAAAAAAAAAA
QWCvUXXHUHfGRGi/OTZkIB8tHx0qHx0oISArHh4pIyEtIR8vIB4qHx0pJyQxJB8vNCo9TjpI
V0NOHR0rHh4qHR4qHR0xISlbJTx/Kzl7IidFMUSPNU6lOlOsPVWtPlaxQFm3Qly4QFu4QFq4
PVayOlWvP1myRWC3RmO3Um6/XnnJZIHNhJjbmKXhnqvmo67nmKXcKC9TLzlhKzJaLC5RJihG
ISNAR2ioYJbdQmaxJSxTHiA5IiNCKCtPLC1RJyZDQl+uUXTETnTDRmrAOjtrIyEwIB4qIB4q
JiQyJCIrJCIvJSExIB4qHRwoJyQyMSg1XkZSUkJUT0FmHRwqHh4uHR0tHR4sHyE6JjJzLD+K
Jjd6JSlIN06hOlOsPlSsP1a0Qlu4Q1y4Pli3PVe1P1q0QVuzQ1y1R2O6UGrAV3PEZH/OcYrR
h53dm6rln63moq/naG+gLDZdLzhkJitSJCVGHh80HR4wSG+vSoDJY5TdSnrJISZTJCVEJSdJ
KzBZKy5VR2S0UHPFUnbIQ2a7MzZzJyM2Hx0qIB4oJCAtJiMvJSItISAuHhwoJiQwNCw4UzxK
RTQ+JiEsLiY4KSU1HB4uHh8tHh4uHh4wHyVOLT6HL0KLKDJqLzluOVGnO1CpPle0QFq1Q1y5
OlSxPVexP1m0Q164SWO7SmW7U27AVnPDbIXPfJPWkaXknKvloa7po7HpOUh1KzJVIyhKIidK
KCtLKS1MMTlgQl6cRXK2VIHEcajtNFKeJStTIihWIyhQMDRgSWm8UnbJVXrKQ2a6Mjl1JiQ6
IB8pJCM0Hx0pJCAuIyEsIyIwJiMyJyMwJyEyRDVALCg5STlGLCQ2X01WRT5PHR8zHyA0HiAx
Hx43KTd1L0OTL0SWKSlTOE2gPlSrP1ezP1q1QVq1NlOtPFiyQFu1RF63SmW7Tmq/VnHBX3vJ
cYrRgZfZk6blorHvo7Ltoa3mMUJ6LDRaMTplOkdyQU99RFJ/R1iHR2OiOlaaSW2vfrHuS37L
Jzh5JitaJjBiJipRTGy/U3fJUXbHO1+0QkZ1ISEwHx4pHhwnJB8vKCUwIB4rIB8qIh8qJSIu
MiY2Oy88WkRLXkhQSTdEPTU/Hh4oHiI2HRs0HCA2HR0yICRPLkCHNUmbJCxfNkOHPlOrP1ay
QFq2P1e0O1WwO1ayQFu0RWC4Sma9U27CXHbHZYDMeZPXiZ7glqjnna7robDrY2ubRl6ZSluH
SluKRlmISluKSVmGR1aCQVaPOFafRnO+fq7vZZvkMEqTJzd2KzRnISlcTGy+U3bGUHG+RmGx
Rj9tISArHx8pIyEuJiMzMi06KSY2JCEuIyEvIB0pLyc0HxonSjdEZU9fU0RUUkZUUktfKylM
NDtvJyM5HR4zHyE7KDh8M0iaLjt7LDZtOE+mPlWuP1i1PFaxOlOsPli0RV22SmO7TGi9Um7C
X3nJaobRgJjbj6Tjmazrna/qnq7oSk9zR1yaU2maTF6NS16NSFmJRlmHRFOAQVOGUnm/canx
d6rsc6frPF6sJjt+JjNqLTt2TW7AVHnNVHrMQ2S6MDdnIiEuIR4rIR8rIiEqRC9BJyQyIB0n
ISAuIB4pLSk2SztJPCo5YEtUVU1lfWiCV1BgFhgoIiNAHB4yICE1ICE1KjFcMUaXKTqAKS1b
OE6kPFOuPVWwP1ewQFaxQ1y2SGC4S2S7VW/CWHPGZ4LRc47XhZ3gkKfnobDtoLDoprfvIB80
Q12fTGOZSl6RSV2RRViIRViIRFKAO06BUHK0gLTweqvvfq/uToDMLECBKDNsKjp1Tmy9VnvP
UnbKP2O4SUp6HiApIiArHx0pICAqNCgyLSo7KCY0JCAuNC4+MSEzTTdHSDZGY0taaFFkGhoq
X1x1HBwpIydFHhwvHh4xHyE0HiE9KjmBJzZ4JyhLMkiePVOrP1WxPVWxPFavQ123R2C3TGa9
V3LEWnfIZ4POdI7WhJ/kkqnqobLsprXsbHqpICE4Q1qZSV+VSl6VR1ySSFuLR1eGQlF/PEpz
P1iUe6nsa6Dpg7Xwap7iME+VKDNmKTZuTWu9UnbIVXfKPmK4Sk2LExgkISAnKCc3Hx8nJB8u
PS9BJCAuOC8/LSQxOCs7SzZFQjBAZk9bcV1uQzxQT1FtHR8kIyRBHyAzHh0vHyA2ICI3LDdx
KzZyKi1KMkSYPFGoPFKsP1WsQVmxQ1uzSWK6T2u+VnLCYn7NaofTc5LcgqDljqfon7HtpbLn
OTtaISM7Lz9vS1+TTWGYSl+URliKQlOEQ1ODRVOAN0l+QmatW4fNfq/sdafoUYDHJSxfJC1i
S2m6VXfLTXLGQl63bG2TJCE5JCExJCEwIB8wJyMzMCc4IB4rJiIuSThIMSQ0QzFAWURVZlNg
RTZHYl2DPTpLHR8mKCpHHB4uICAxIiM5Jic+KTFjIihdMDFZO0yYNkmYP1atQliuQViuRl20
UGq/Vm/BYnzLY3/KaIjTdZXehKPoj6rpoLLsWl+DHR83IyZDPUh2S2CTS2CWR12SSFmKRliK
RlWFQk95QE13Mk6RPF2jfq/ue6zrZ5fYLUJ7JytTT23AUXLGTnLIQV61XWCRPEBpISErKykz
LSg8OzBJOi9DKiMuMio+SjtLMCIyRzRDalJgaFJZUVFwOi1FKChFHR4pJShFHRwuHh4wHyEz
Hh40JipRIiNBLTNiM0ONNkePPlGfQ1muQVesQ1qwT2a4WHLBZH/Jb4vTdJLYfZvgh6LkkKfj
eoCjIyc+HR46JCZIR1F3RVuTSl+SSVyPSFqNRFKCRVGAP0tzNj9pMUqHLkSBa5rcd6rqcaHh
TXK1ISE8UG3AU3LHVnTFU23AKipTQDxVIB8pJyQyLyU3KiQ0NCw/KyQyVkBQUz9NVkROSzlH
SjNAUz1STUJZOzFIICA+Hx8tISQ+Hx8wHx8zHh4wHyA1HyA/IyU+KC9gNkWMOkuYPE6bP1Sk
RFmsRlutR12uVW68cIjOe5XWgZvdh5/fi5/ZR01qIiE6IylIIyRFJSlNPkBhP1WTSlyORFeM
P058OUp/Mj5rLDVfKTBWMUBuKjNdV3m7b6TreKbnZJPXPlqZUG7AVHPFUHTHQFqqYWeUKiFD
JydEKSM1KSU4KyY0Ix8sVz1KXURPJyExb1RbUT1LZExRQz1RXlJrHx4rISE6Hx8yISQ7Hx8w
HR4wHh4xICE2HyA8HSA6KTJkNkWTOEmYPU+fPFKdSF2vT2OySV2pUmaxaoXEf5fVd4i5TVZ5
ISEuHyAzISM5ICVEJihKISZIKypEQFKQOUl7MUF0M0B0LDloMTtmND9pO0VvQEhvQklrZYjI
canvb6Lmc6PkYYzTUW/BVXbIT3PIPFquIytXTERnNjlRKSE4KCQzKSM0IRwocVFPPTA+QTA/
YUlRY0tdW1ByST1UHR4pHR4rICI3ISI2IiM6Hh0wHh4wHh40ICI5ICI9HiJDICRGICZTNT16
N0aCOEqLTFusVmm7WGy7XG29XmqlMjdgHxw1ISAzIyI5ICE1IiM6LzVWJilLJCdGIyA9Qlyd
MD5qNkV3QEt/RlWDSlmFSluISlqFRlN6QUpvYHyzhLXyea3sdqfpcaLoUW/AVHbIUXTGPVqs
KC1gKipSLClGNi5GJB8tKCExKSEwc1RVRzRCZEhQWUJMX1J6KClaLjBUICAyIiAxIB8xICE2
IiE4Hx40Hh8xJCY8IyQ8HiA6HyJGHyJLJzRxNEWZPVGmQVeuTGK3X3PGXG/AXnC/UGGqLThw
HR00IB8xHx83ICA5IiI5MTZbJytNHiE8IyM7WXCvRlmOTWKVTWGQT2GRS1+QT2GQTV2HR1WB
TFZ9T2WWksD3fLHvfrHwg7DuTm2/VHjMVHjMPVywIydaODZdNTBTLCU3JCAwLCcyKSMwmXJo
VT5CW0JSSjZGRj5iPT1me4rgISdKHx8uHyAxHB0xHh0zICEyHyA4IyQ+ICM+JCZGIipdLjl7
MkKQOEyiQVWsRVqxSFyzTWK4VGq/VWy/T2OyQFOfIB42Hh4wHh8zHyA4HR41KzFSJypOJitF
IiM9W3GvSV6UUGicUGaYTGGUU2eXUGOUTFyKTVuIRlR+QVSBj7v2irv3h7TxhLDuUG/DVXnO
VnjNS2O2Ky9hPDtlOTxvKyg8NTBHOCo4KiMxVjpETztFRzlJUjVBR0BaKz6AYmGeSGGcHCA9
HB4uHx8yHR4yHx82IyM+HyE6KClOIidQKzd7LTyIN0eaOU2oQFSsQFaxOk+pNUmfNkqjO0ul
QlSmR1ajLDl3Hh8xHBwxISE6HSA6JitHLjJVICJAJSZAVmypSWKgT2idTmWYTWScT2WbTmKS
TV2PSFqMR1aIRliHgrPzirn2irfxjLfyVXXGV3nMUXfLQletQUB0JylWS0iDMSxONSw9JiAt
OCs8SztFQjBAUz5MQzlKSTlKPlWjcX2zOUp6U1mGISExHx4wHyA4Hh80JilDHyA3JypPJzBo
LUGQM0WaOEynPlOvQlu3OVKxM0utN024Tlq/cnrQcXzPSVWrQFWdHyVJHR4xHB0wHyE8HCE6
JSZLGx06JyhDR16aQ2GlTmmkTWScUGaeTGKYUWSXSluRTV6SSluOSV6TcqbthbTzgrHvh7Lv
UnPFUHPGUnfMPlyxJy9oRkJ4ODpyNy5OOi0+JyExLCczQDE+RDFBQzFDSzxLTjlHREB0hIGs
d32sKCpaNTZgHhwxIiRHISE4JidCHh41LjBbKjZ8N0mhNkqkPVKvQVezQFayLj+NLDiALz2O
LjuGJTaAP0upNEOXN0iQMkGGHh8yICA0HR02HSA8ISRGHiI7NTpZOkyAQ2CoTmilTmWfTGGZ
T2KXTF6SSl2TSV+VS12TRViMYJThgbLwfq3sfq/uTm7DVHTKVXnNP12wJy1iJypcQ0F/PjVY
Tz1NKCQyKyMzLCQzNys7Pi8+XUJYRkdqHR0uODhVWVyDTmKcR1GHHitRJiRBHyA2JydCGxw1
LTJlLT2MOEukPE6rPlGyPlS0Q1i1Rly5SV22RluxT2O6WW/Ic4zceILPU2W1RU6aIChMGxwx
HB0yJCZHIyZGISM+OT5lPkRzRWClTWajUGafT2OcTmCYTGCYUGKYSl6WR1uTRFaMVonVc6fu
fa3udqnqTm3DUXLIU3bPRF+zJy1gJitfNjdyOjNeLig6PzJCJB4tQS9BUTlIaE1XWEBPLic5
IixTKSc2NzZMWl6COURyTWGXHhwuHiAzIB83Hxw0KDBsMECSNEiiOUuoQlW1QVe2SV+9UGXA
VGrDUGW7SmC0Vmq8d4TQiZnaWW68RFSkMTqAHh80HR4xJidLHB4+IyQ/Uld+VVuFP1iaTWSg
TGWhTmOeTmObSmCYTmGZS1+XTF6TRFeHVoTOYZnleq3ve6rtTm7DUnTITnPIPFiuLCpkKy1l
Nzt2PTVjNSo8RTlTSjlJNSo5RS45WUNUUUNHHx4uHjFIKyYzMDBWLChEKTFPSF2ZbXSVIR81
Hx8yIx02MjpyKzyLOkymPU+tQ1e2SF26TmPCUGXCUmi/S2G2OU+kOk6nOEqYZXK8W3G9RFem
OEeVISNGHx8yISNAHR8+ISM4U1qJQkZrO0uCRmCgT2ikTWOfUWWcSl6WSV2TS16WSluSQ1eI
T3a+X5fmZ5/oeqvtUHDGUnXJUnbON1OoLC9ePjx3NjduNS5hNyk8SzdLMyQzNy9AVz1JWkRP
JiEzHx4uIyMzHiIvMD+GJylHIyMxVGWNQVOEPklzHh8wJCE5IildLD2JN0mkQlSxRFm4SF69
TWO/UGfBUmm/PVOpPFGoNkqgNEimUmnCeI3TRlurPU6eKTV0Hh4zIiQ8ICFBIyQ3V2OYNTdX
MzlbQFmaUWilT2aiUGWdTGGaSl6VSF6XTV+VSluTSmahYJfkXpXjbaHnUW/CTnHFTnHJN1Kr
JipdODtvU015SkRwPzVLRDJRNSo3TjpGYEJKSTpWIBwtIB8rIyEvLzhlLzZqSkmDKSVIGh8o
MUiDh5O/IiVHISM2IydWMD+OOEukP1KvTWPBUWfGUmnDV27HVWvDP1awNkmgNkuiOU+nY3LH
sLnqTGKxQ1WlL0GNHiFBICA7JCdIHiA5Y3GkIyQ/ISI8QFeWT2mnTWemSmOgSmGcT2SfSl+Y
SFqQRlmPSFuPXo3VW5XkXpfjUHHFU3XITHHHOFOuJShaMDRlNDV4U0+FSDxLQzpUPTFEKx8s
SzdDNCczIx0vHh8rIB0rLCs/JyQ7OztoPUuHHR0sKjViZnKpcXuiICAwJCpVKz2OPU+pRVi0
S1++VGrIYXfQYXjQXHPKSWC7QFSsOU2mPVOugI3WipPOUGW0SFirNkeUJStcHR03JylJLS5J
VmWbHiA1IiM3QFCISWGbTmqrUWilTWSgTWKeTF+YTF6WS12URVaLUm6rXpbjWpPkT3HFU3bL
UHTJOlWrJy5gJy5mND+CRUB/RDhSLiM2KyU2PS9BPik7QDJFHh0qHh8sIyEvHRwsJiQxOj1q
ISY6Hx0uIx4vPUuHaYK/SEp7ISRRMEGVOU2nSFu4U2fCXHLMaXvRZXzOX3XJTWS7Sl62PlOt
RViwmZ3ggpLRV2q5SFisOEmaJzBwIiI/IiFBQEFiTFKDIiE1IiE2NT5kR1qSUWqnTmaiTGOf
SmGcS2CbTWGZS1+VSVuSRFWKV4HIXJblUXPIU3nPVHjPOlaqKC1hJjBpKjZ6VFCFX1V2SDln
MSU6JSExSDpLRjFELCE1JCIuOys6V0FNPjRJMTBLJSM3Hx4vHx4wMTReU2OghJPDKzZnMUGN
P1KnSV21XXDHZXjOZ3nLZ3vMZXjIU2e7RlmvPE+rRVeyp6rlgI/SVmm2QFKnOEaXKDJzHx8/
IyFCPD9lPUFqIiE0IiE1LjBKQ1aNUmmnU2qmTWSjS2CbTmSgTWCaSVySTF2VR1iMSFyQVoTK
TXHGVnrRUnjPPVmsKjBjKDJqLTyANkGMUlaJQkJoNStMJyM5JyIwVT5KVj5LKyc0NSs4Pi8+
YUdWMCtBHx0tHx8vIB8xHh4uQEqCa4LAe43FKDmCPEueSl6yU2S5UmW5UWC2Wmq1W27AW2u5
RFSmPU6oT2K8qq7pdILATF6rO0iYLDZ4Ji9nHx8+JSVCPkRpOTxgISA0IiE1Hx4zRlaJTmSf
T2elTWWjTmWiTGOfTWKdTWScTWKXR1uPR1eKTWKUUHTKU3nPT3LHN1GlJixcKjRtKTZ0PEyJ
KipRNDVRV1eYaHeoNS5ZNTFBNCY5MCc4LyMzUztJQzJCJSAzICAvIR4vICAxHx8wJzBXWmeb
cX+3eIG3NkWTQlCgR1epNUKMPkmNWF6eX2GnUVyyOUqaOk2kUGW7rbLtSVSQR1CUU1COJC1g
HiNKHh46JSQ/SUptNzlfIiA2Hx8wJSU5PENuRliPVGupUmmnTmWhUGeiT2SgS2KeUGWfTF+a
S12SR1iMTnLIUnfKS23BNU6fJSdTJSxhJzJxKjZ2LjVvJi9kX2+pQ2GxQlekJy9XJCMtMCY5
Lik7QDE+Nig8IB4rHBwqOzRDHxwuICMyIyM0MDphZHCegY2+cYC3NUWNLDNfKCtUKypLRkNd
l5nOOUOMMD+KOUugUWi9mKXhM0KCKCpGNzZYQUqIHR83HB4zJyVAT1FyJiVHJiY8Hx4xIiI2
LC1KRliST2ikTmelUGWgTGShTWOjTWSjTmOgTWSgS2CVSVuQT3HEU3fMTG7CNU6fKCpUKC1f
KTJqKzZ1Kjd2JzRyVl6RQFOdRV6uNUN8MTJsKCQwPCtHOCo+Tz1SJCAvMio7HhwvMSo3RTdH
HRwwKSc+TF6ZXHi4iZK+anOxIzBoIiNEHR83JCQ6NzdVJixUOU2hPFCqU2zEgJDabYDEIidS
ISA5KitIHiA5HyA6IiI3Q0ZxJydHKSc/IB8zICA0IyA1Q1SLSl6WT2enTmalS2SjTGWlSWOi
S2SfSmObTGObS2CWTnDEUnTIUHXKNk2dKSpUJSpYKTNqKDNxJzNyKjZ2KjiANUONP1ahOUeK
LzJTMD9+ICY/PjFDMyY3RDJEICAtISAxLyU5Hx8uMSg7KiU9KjBRZ3WxdIW9cYG0aHqxOkyM
NkWPOkmOQUyYKzV7LTd5S2K5WnPLdYvaeIrXW3HBMT55JSxeKjZyJSdLJCE5Sk16JiZBJyY7
IB8zIiE0IiE1PkZxR1WITmmoTmenUGimTWWjS2OeUGijTWWfTWWaTmKYUXDCUHPGTm/EM0qX
KClSJipbKzZsKTRzKjRyKjd4Lj2BNkaNMUCPMkaGKjJhdXurNEOCLytLLSY6SDVEIiAvJSI4
JSQ1Hh8vLio5LSY8KCU4NkR+UGmmbH+3Z3WshpK5T1+lOEWCPEWBRlabZnjLZn7UZ3/UeI/b
d43ZhJjiepLhVmiyLDJqHh02JyY/VFmIHB00IiE2IiI3Hx8xIiA0MDBMQVCFT2ahT2ioUWql
T2afT2efUmqkUWWdT2WcUWaZUXLGUXPDUHHENUqWKChUJipZKzRsKjVxLDZ3LDh6Lz2BNEOR
MEOWMUaLMEN5MD+JSkiAQ0J6JiQ2Nis7JCAwKSQ3LCU6IB4vIhwtLSQ5JiExIyQtNj95XW6s
X3azgY29bnynZXSyWmy7XXDBZnnMaYDUdYvbh5vkh5njjJzil6fpj53hNEaQIiU9JCQ8TlWD
HyA3IiI3JiQ4ISE0IB4yJSI1Q1CESFmRVG6rUmukU2ujUmylVGujUmecTmGTTFuJTnDET3HE
T2/EOE2aKCZTJitcJzBqKDNvJzJxKTN1NEGKMj2FNEOQMkCDUXK4M0KGP0mMRE+XMjZ4KiM8
IB8tLSU6JyM1IB8wJCM2JCAxRTdMQThMJzFOUGGeZoC8cYO3ZGqThI+9aHeuWGy1X3XGaH/S
fpLcip3jjaDklaPmmKbnfI7WLzh3ISA0KihEU1WFGhwvHyA1IyI2ICA0Hx8zIiE0OD9kSFqM
UWupU2ulU2idUGWbTmGWTl+PU2OWUF+STnDEUXTIS2zAN0yZJiNQKSxdLjdwJDFtLTZyKjV1
MTl9Lz2GLzyDSV+cYYjXLzt7NkOFPkuSLjx9Ky1ZKCpJKSM5ISAwISAxLig9KyM4HR8vPTBB
ISAzKDFUS1mYYHe2b2yLc3+vbnyxaHWlX3CyZHfId4vWgpXbjJ7jlKXnl6jrX3TCJCpaIyM4
SEZgREZsHh4xHh4yJiU4JSQ4Hh0vKig8LCk/RVSHTmGWUGOcUGCWUWObUWWdU2iiVWqlV2ul
UHHGUnTISWrANEmXJidKIyZWKDBnKzRvJzJsJjFvLzt9MT6FMD19XXCtb53pNU6SPEiEOUSF
PkaNOkydMTVrLy1RJiQ3Kyc6MSpBKSU2Myg7PjFEICAxISA1LCtESEl2fICVbX+3doS0Y3Kn
dICxYnKqbH29gZTajp3hipfbhJXaRFimJCpNLy5MR0ZnPEV2ICAvHBsuIyI2JCI2Hh0wISAx
JSIzQElyRlJ8UWWbV22pVm+rV22nUmekVW2qUmekT3HDUXPGSGm/N02ZJiZLKS5ZKjJoKTRu
JTFuJDFuKzl8Kzh5LTp2dozFWoXNPVCXOkeJQEiGPEaDVU18S06LPD1yNjBYNi1JKyQ7IiAw
NCpDIh40JiI6Jh81Mig8U0BUW1V0bXigdoe8c4O1bnyxcH+zcH+veYe3hJDQhJPZfI3PNUKL
LS1cPDtZT09zQFSYHh0xHB0tIyM2ISE1Hx8yIyMzJCI2MC9LPkhzUGKZWHCrWG+pVm6pVGqn
WW+oVGqlT3HDTHHERWe8MEWQJSZLKCtcJzBlKjRtKjRuKTRwKjl7KTd2P1mNaazyVXe+SmCi
O0uMPkuMOUSFRE2KQ0uPPkabMDJgLSlJKyE3KCM3KCU7HR0tMyxKKyk+QTFKLyU8QjdKVlV/
d4i9fpLFdYW1dYOzan2wc4Cxfou5c4G2XGyrKCxVKipJSEZkSk14XHa+UG6uISlMHx8yIyI2
Hx4yHyAxIyE0JyI2P0h0S1iOWHCrVWymVGqnVW2pVGmjUGajTXDDT3DCR2m9L0SNJyhMJSpa
KjNqJzJrKzVwLDdyJzV0KjVsZ3y6SX3lSGq7O1WmMzx5QlKXPU2UPkqJQ06WMjxzQk+SOkaO
PT9yMCg/MytDIyI1OzFDNClJNi1DPDBHKSU6SkFOOk6Lc4G1c4a8doW3eIe2dYa2eYi2e4y6
iZa7XG6gPT5lPTpYWWSbTW6+VXO+Y3m/KjRWISA0ISE1ICAzISAyJCI0OTxeTVyXU2elV26p
V2unVGqmV2ulUmajTnHHTm/ESWq+L0SOKitRJy1cJy9iKjNvKzRuLDd0KjRyKjRshrPtYnzN
PlKsNEqMMj+CMjx4PE+WOUaIPEiMQU+VPk6PQlOcQESCQ0F8QDZXPTNWKiU8STdMRjZPT0Fe
MixBHiAyIx4wUlqQa3SrboXBh5bFhJLBe4y8gpHAi5fCj5jGjZjFbnSiMzdjLzVhO0l/T2Sh
Zn/CLTZXHx4yIiEzIB4xJiM4JiM3TFqXTV2bVWuoU2ilU2mlWWylUWWgT3LFT3HESWm/MkOM
JidNJyxaKzJlLjdxKTRtKTVxKjZ1MT5yUZPvZpHWOFWhO1ipOUWIOEF+OkV/O06TQVCXPEeK
QlGSRVScS1WdU1SaRkZ5QThlPTBSWVl4Yk1mSTtYQDRILCY+ISA9KyY+U1qDXm+tg5XHhpfJ
gI+/g5O/i5nEj5zHk5/MkpzJjZnQWWWXMDp0KTFYOkZ8Zn25P0RiISAzHx4xHx8zICAyO0Fq
SlqbVGmmUWipUWenUWWkT2KfTW/DTm/DQ2a8LkCIJylQJCpaKDFmKjNsLThtKTVwKTVyUVuO
cI7XPWzRQF+sNUykN0OFQE+aRk6NNkKFQ1ekRVSgRFGVRVaZTFqfSVeUUVSNVFCHSE6HY2uY
VleCTVaFWmepXWajZG2tS1CDUlWAYGukb4G9d4u9iZXGhpTCipbCjprFkJ7KlKDNlqLOlZ/I
mKbPVGWlND5vPUVweIvBHB8xHx4yIiEyJSQ5Kic/SluZT2CfUminUWepTGKgUGakS23BTGy/
RGa6MUGLJyhQJy5fLDNqLDZwKDNqKDVxJjRwPD95WJPYP1e0PFm6MkaUKzl0PEmMQE+bQE6K
O0eKQVakQFKfSFObTV+ZUWWrUGSrYGq0am+yamukYHO8VmapX3SuXm+0XGyyXm6zZ3CpYXCu
XGmhaoCxdYbEhZPIfZPJhJG/jJjDkZzFk5/LlZ/In6fOiZTBXm6cT2GgaHOhQlByIR8yICAy
ICAzJCE1QENtS1ycVGilUmioU2qqUWWmUHDETnDCRWS4MUKHJyhQKC5fJzJrKDJuKDNsJjJv
Kzh5KDBqUpXuRGexN0+VMj+CMDl1NkOHNUGERFalVGCnO0yTUGGsS1mgUF+jWmasVGGjYnfB
YW6wZHSyYHK4ZG+yXHGwYHazaHizZ3i3doO+a3uybXywdH60cXuteZHDiY6/lKLMiZjHiJbD
jZrEk57Ik5zIoaTNany1X2ylU2GWXG2eIB8xISAzHx8wISA0LCpETFuXUV+aUmmnUWenUmeo
TG7CT3LHRme6L0CFJidPJSteKjJqKjVvJzFqKDRzJzR5KjVwUIjkN1esLj+EMDuAND19OEaE
O0aQPEmIRVunQVOgQlaVU2WvTF6dVWOqY3S5ZHW0aHq6XHO6bX+9anq9XXG2an+/aX3Fc4O/
doW6gou9cH61aXu3c36ucXqncoCwe42/hJHDk6TPkqLQj5rGkJzJlp/KkaHNY3WtiZfCmqLN
IyQ7ISEzICAyIB8yJSI2ODldTV6fTmKfUmmsUWepS23BUHHER2e5MECFJidPJitdKzRqLTdx
KTNtKTVyKjd5LTp3aJbiN0+iLDh2Kzd5KzdxMj18NkKJOEWLPk2PSF2pR1mhSVyeUmmyXXKx
Wm+3b3a0aHq2ZnK4ZHe/eITJYHy9eYjAbYTDaX65g4zCfYzAgou5hY3Ag5C+h466gomxlJTA
cYK+g5jOk5jOkqPQkaDJk5/Im6TOkZ7Jm6PKr6/VWWOIIyM1HR4wICAyIyE1KCc+R1CGTF2c
UmmnT2WoSWm7Tm7AQ2G1LTuAKChPKS9dJjBoMDt2LDhzLjp5Kzl+Lj6CaHq7NkeRLDRsLjqA
NEB/OESKNUWDPUuWMkCIOUiKUWWvS12nU2uvZna5aX2/bn+5cYXIg5LMbnq0donGfIzFe43H
jJbJpKXNdIe5eI27jZnJmKTRlJ/Ll5vEoqLHdoW5QWS8UmaxcoS9hpvJl6TNmqnSmKPPmKXQ
n6nPqK7TaGqFIiI0JCM1ISAzIB8zJSM3LCpFTF2cSVuXUGeoS2u8SGi6P1yuLz1/JiZIJChU
KTFmKzRuKjRxLjl2Kzh7MT+CT3K2OlWgKjVxMDt8KzdvN0WSNkaPMkGBOEmUNkiNOUSGY3Wv
XW+zWnG4a3zBcoLGeY3IZnu3kY/Ae4bAi5nMkJnQlaLVjJnNmKDQgZXCk6PPmKPLoafPqavQ
mJu9N1ShRmW6P2CyM0mRUE5yeYKojZi+o7Hal6bRkZzEqq/Ta3KhIB8xISE0IiEzHx8yIyI1
IyI0PUJoTV6bTWGdSGm8TGq6P1ywLDp8KChIJilVKTFnKTBmKTJnKDNtKzZ2Ljp9R1SOPl2t
LjdyLzl5NkOBNUORPUqIOEyXN0OJPlCWQlGVOUh8YW+vYHm9bHq7dYG4Znq6fZXLbYe/nJzK
hZHAkprJlaPTlqHTj6LQl6XQoKzToKrQqK7TsrPQLDdsP1mnRWW4RGO0OFGcJCtWIyI1bnaN
jJq+srrWqrXbo63Tb3GUIB8xIB8yIB8yIB8yHx8vJiQ2JCM2SFWJSFiRSmm8Smi7QFywLjt9
JSZJKy9eKjNnLjVsKTJsLDdzMDp8Mz+CMTp+T264Ljl3LTh4MT1/NUODO0eLOUqIQFGTSFyg
PFCTPEuOO0yTVl+UdIXGb3+5cYrGZH+9e4vIk5rIi5nLoqbXnKnWpavRm6bQpq/aoavToK7U
v77gaG+TLj55QFurRWK2Q1+wOU+bJCpXIyI3IiE0S0xkmajSoKvPubbOSk5zISEyIB4xISAz
Hh0vISAyIiEyIiE0KilBTFqVS2m8S2m6QFmqKjd8KSlNJypdKTFmKjNrKDJrKzZwMD19Lj2C
M0GKN0+UKzh6Ljt4ND2BN0CGNEJ+OEaLQ0+NSViqQVCVRFOaPk+dQlGUUVyccH6ydIfEgJbO
cIvJiJXLoabTm6zcoazUpq/Wn6nUoazUoqjQsLfblpy9MDNfLz5/P1mpRWCwQl2tO1GdJixZ
IyE2ISM7KzduO1KjQFWgPlehOE+cIB80Hh4vIB8zISA0Hh4uIR8zHx4xJCM0MjNSTGzASme5
QF2vLTl9JidLJi5ZKTFlKDFrKzRtLDVxMz58M0KGMUCIMkKMM0SMLj19NUKBMkGFOUSFN0SF
PEuFPU+WQ1SWRlOXQ1mjRleUSV+nVm2qcX2xfpHHkaLRmKTTiZbHmp/Noa3dmaHLo67XpbLd
pLHVl6DANDljJi5eM0WKQl2uRGGzRmCwOlGcJStYISA1Ji5bO0+bQV2xO1SkOlWnQFqpLz96
HR0vHh4vISA0ISAyHyEwHh0wIiM1Kyk6RGi+SGi7PluuLTt9JSdMKS5hLDNnLDVvKTVtLTdy
NEB+NUSJNEOPNUaOOUmRM0KGMj97NUKDN0OANkF5N0OAOEiIQ1KNQFSbQlWdSVmeTl2dVm2r
c4K/eICyl5vDmKTTr7Dcpq3ZoKvSo67XoarUt7rakZnAJCpVJS5eKzRqNkeNQFuvRmK2QFyu
NUybJSpVIiREM0KEQ1ytP1qsPlelPVanPVeqQlquIylOHR0sIB8yIiI0Hx8vISEzICEvIiE0
Sm7BS2i9PVuwLDp/JidNLDJjKjFlKzVsKjRuKTRsMT9/MUCFMUKKNkWMNEePMkaRLDp+MD16
OUaGP0qRPkyMOEaKQE6URFaUQUiDSlqiTV6hXHOzcYW8d4u+bIS/jZXEoanTqbHcpq7YoK3X
oK/boKjHKDFeLThvMDlwKjRtPE2TQF6yRGK3QF2xO1GeJClYLDRtPVOhQlywPlmsOlOkQFur
OVWlQlqsN1ChIR84Hh4tHyAxHx4yHh0tIB8zHB0rTHDDS2y9PF2vMD2CJihNKS9fLDJlKzVn
JTBnKzVyLzt8LDyBMkGGM0SLNUeQMkWSPlSeNEOBMz1/NkSBOkeLOUePOkmKQ1CVR1KWTl6i
TlmSXXGndoO8gpDCeI7Fh5fGgJPJgYu6kKPLqKvJsa/FSFN/LTl2Ljx4MDx6Lzl1NUiQQl6z
RGO5Ql+yNkuXLTt8Ok2YQVytP1yvPluvQVusP1mpO1apPlurPlusMUSHHR0tHx8tIiE0Hx4y
ICEvIB8xS23ARWe5P16xLDuAJShLIilXKjFmLTVsJjFsLTh0Lzt8NEGIN0WJNESLNUiTN0eT
NEiNOlGPPUiLPEWBPEaERFCOQEqMSVaUS1aTTGCUXmyzbnmuZ4K5dou5forAgJLGgZbMfpPB
h5nFWGOTLz17LDt6MkCBLTx6Ljp5MDt4M0aOQFutRmG1P1mnPVSfOlCbQVuqPlmrP1quPVuu
PlenO1akPliqPFmoQF2sQmCvN02QHyEwHR0vISA0HR4vHh8uS2/CS2vAPV6yMEGFJihQJy5f
KzNqKjVwKTVzMTx6MkCHMUCJNESPNUaPNEeSO0uXOUiZPEeYVFyWRFCMPkiHQE2DR1GKQ1eL
UmCgWGKsWWebZnusboO3dYW8gIzBe47IepHDcYGwRlGJMUCGNESLNEWNOEeLMT+DMD16MDx3
OUySQVqqQFurOlalQVqnQFqqQVytPFemP1uvQFurPFikPlupO1akPVqpPFmmVHzLh7bzPVGP
ICAyHRwtICAzICAyTGy+R2i8QGC2LDuEKSxaLDVtLDdzKjVxLTh2NECDNEOLNUaQNkmTOUyY
NUaSOEuXO02cO0+cO0ucOkubQVCbSVeZVl2WSlmUV2GgaHKrbG6ocX2tb32tZnqwdYO2fJG+
U2OWMEOML0GLM0aRNEaRN0eSNkeKM0OFLj9/MD18NkqUPlipPFalP1mnQlypQ1+wQ1+vQFus
QF2wQFysPVmnOlSkOFSkQFuoQGCve63xkb73VX7DIyM8HR8vIiIzISE0SWi8R2W5PVyzMT+D
JilYKDBqKzRyLTZyLjdzLDl7MD+HOEiROUmVPE6ZN0eVO0yZOEqeOEqcOEqbO06eOEqdNUqf
PE2aQFOXSFSVUmCbanOfdXuzX3atW2GeRFSONEOSMUaVM0qYN0uYOEmWOkyYOEuYN0mSM0WJ
M0KELzx7NkiQQFmmP1mnRV+tQmCwQ2O0Q2GxRWS3QWG0P16wPlqrOlenPlupPFupY5Lfibz4
cqLjM0R7LTlwICE5Hx4xHh4wRmW6SGe6Pl2zLj2CJyxcKjJsKjRwLTh0LDRvLjh3NkOJMkCJ
N0aPMUWQNUiUO0yYOEuZNkmbO06gOkyeOUueMkebNEudN0ufNkuZMEWTMkeWMUSQNEaOMkOT
NkiXN0uaNkybNEqbN0yeNUqXMkeUOEyYN0mVMkSNMECELjt8M0aQPlaqQ1+vRmS0RWO2QmK2
RGS3QmGzQ2O4QmK2P1yvPlusPlysT3bIhrf3iLn3QF2bKzhzMkF9M0B8ISM8Hh8vSmu+R2e6
RWS6Lz+HKi9iJzFsKjV1JzNzKjRyMTl7NkONOEmTMkSPOEycO0+fOlCfOk2bOE2fOk+hOEqc
NkmbNkqbOE2hNkueN0qaNEaWNkiWNkqWPE2ZN0mWO06bNUqbNEqcN02fOE6fNUmaNUiWOk6a
NkqUM0OKMj+CMT5/O02aQ1ywR2a7RGS6SGe7RGe7R2a5R2W6SGi+RWe7QmK0P16vQWCwcKXu
j7/8YY7VLTt0L0B/NUaFOEqKMkKBIiQ/S26/SGm7RGO4Lj2CJy1eKzVyMTp5LTh1Ljh0MT18
Lj2GN0iPNUmUOk+dOlGgO0+cPFCdOlCeOU+fPVGhPFGgO1CiN0yfOE2iNkudNUuaOU2bOk+b
OUyYOU+eOVCfOlGhN0+eOU+fNk2bOE2aOE6aN0yYNkuTOEmNMj9/MD1/Ok6ZR2O2SGjASm3C
S2vARmu/Q2S6SGi9R2e5RWS3Q2SyQmGvV4TTgrf5eazwNkyGMEJ+NUeEN0mIN0uJNkqLN0iM
TXHESm/DQGG2LT2AKC5eLTZvMDp3MDt2Lzt1MUB+M0SKO0qQOE6WPFKdOlOePFOfPVSiO1Og
PFSkPlOkO1KhO1KfN02dOE6fO1GhO1KhOlGfOVKgPFOePFWgOlOgOlKdOlGeOlGeOVKdOlGb
OVGaOFCZOE+UNEmMNkWDLjx6P1eiR2i5THC/SGzASW7DR2vAR2m9QmW5SGe3RmSyRWSzS3LA
dqvwgrf4RWqpLkB2N0eANkmFOUyGOk6GN0yHOE+S'
			base64Decoded asByteArray readStream
]

{ #category : 'testing' }
JpegTest >> test16bpp [
	| form n read |
	form _ JpegTest lenaColor64 asFormOfDepth: 16.
	self assert: form nativeDepth = 16.
	form writeJPEGfileNamed: 'test.jpg'.
	read _ 'test.jpg' asFileEntry formContents.
	self assert: read depth = 32.

	n _ form boundingBox width * form boundingBox height * 3.
	self assert: (form pixelCompare: form boundingBox with: read at: 0@0) / n asFloat < 8
]

{ #category : 'testing' }
JpegTest >> test16bpp61 [
	| form n read |
	form _ (JpegTest lenaColor64 asFormOfDepth: 16) copy: (0@0 extent: 61@61).
	self assert: form nativeDepth = 16.
	form writeJPEGfileNamed: 'test.jpg'.
	read _ 'test.jpg' asFileEntry formContents.
	self assert: read depth = 32.
	self assert: read extent = (61@61).

	n _ form boundingBox width * form boundingBox height * 3.
	self assert: (form pixelCompare: form boundingBox with: read at: 0@0) / n asFloat < 8
]

{ #category : 'testing' }
JpegTest >> test16bpp62 [
	| form n read |
	form _ (JpegTest lenaColor64 asFormOfDepth: 16) copy: (0@0 extent: 62@62).
	self assert: form nativeDepth = 16.
	form writeJPEGfileNamed: 'test.jpg'.
	read _ 'test.jpg' asFileEntry formContents.
	self assert: read depth = 32.
	self assert: read extent = (62@62).

	n _ form boundingBox width * form boundingBox height * 3.
	self assert: (form pixelCompare: form boundingBox with: read at: 0@0) / n asFloat < 8
]

{ #category : 'testing' }
JpegTest >> test16bpp63 [
	| form n read |
	form _ (JpegTest lenaColor64 asFormOfDepth: 16) copy: (0@0 extent: 63@63).
	self assert: form nativeDepth = 16.
	form writeJPEGfileNamed: 'test.jpg'.
	read _ 'test.jpg' asFileEntry formContents.
	self assert: read depth = 32.
	self assert: read extent = (63@63).

	n _ form boundingBox width * form boundingBox height * 3.
	self assert: (form pixelCompare: form boundingBox with: read at: 0@0) / n asFloat < 8
]

{ #category : 'testing' }
JpegTest >> test16bppLE [
	| form n read |
	form _ JpegTest lenaColor64 asFormOfDepth: -16.
	self assert: form nativeDepth = -16.
	form writeJPEGfileNamed: 'test.jpg'.
	read _ 'test.jpg' asFileEntry formContents.
	self assert: read depth = 32.

	n _ form boundingBox width * form boundingBox height * 3.
	self assert: (form pixelCompare: form boundingBox with: read at: 0@0) / n asFloat < 8
]

{ #category : 'testing' }
JpegTest >> test16bppLE61 [
	| form n read |
	form _ (JpegTest lenaColor64 asFormOfDepth: -16) copy: (0@0 extent: 61@61).
	self assert: form nativeDepth = -16.
	form writeJPEGfileNamed: 'test.jpg'.
	read _ 'test.jpg' asFileEntry formContents.
	self assert: read depth = 32.
	self assert: read extent = (61@61).

	n _ form boundingBox width * form boundingBox height * 3.
	self assert: (form pixelCompare: form boundingBox with: read at: 0@0) / n asFloat < 8
]

{ #category : 'testing' }
JpegTest >> test16bppLE62 [
	| form n read |
	form _ (JpegTest lenaColor64 asFormOfDepth: -16) copy: (0@0 extent: 62@62).
	self assert: form nativeDepth = -16.
	form writeJPEGfileNamed: 'test.jpg'.
	read _ 'test.jpg' asFileEntry formContents.
	self assert: read depth = 32.
	self assert: read extent = (62@62).

	n _ form boundingBox width * form boundingBox height * 3.
	self assert: (form pixelCompare: form boundingBox with: read at: 0@0) / n asFloat < 8
]

{ #category : 'testing' }
JpegTest >> test16bppLE63 [
	| form n read |
	form _ (JpegTest lenaColor64 asFormOfDepth: -16) copy: (0@0 extent: 63@63).
	self assert: form nativeDepth = -16.
	form writeJPEGfileNamed: 'test.jpg'.
	read _ 'test.jpg' asFileEntry formContents.
	self assert: read depth = 32.
	self assert: read extent = (63@63).

	n _ form boundingBox width * form boundingBox height * 3.
	self assert: (form pixelCompare: form boundingBox with: read at: 0@0) / n asFloat < 8
]

{ #category : 'testing' }
JpegTest >> test32bpp [
	| form n read |
	form _ JpegTest lenaColor64.
	self assert: form depth = 32.
	form writeJPEGfileNamed: 'test.jpg'.
	read _ 'test.jpg' asFileEntry formContents.
	self assert: read depth = 32.

	n _ form boundingBox width * form boundingBox height * 3.
	self assert: (form pixelCompare: form boundingBox with: read at: 0@0) / n asFloat < 8
]

{ #category : 'testing' }
JpegTest >> test32bpp63 [
	| form n read |
	form _ JpegTest lenaColor64 copy: (0@0 extent: 63@63).
	self assert: form depth = 32.
	form writeJPEGfileNamed: 'test.jpg'.
	read _ 'test.jpg' asFileEntry formContents.
	self assert: read depth = 32.
	self assert: read extent = (63@63).

	n _ form boundingBox width * form boundingBox height * 3.
	self assert: (form pixelCompare: form boundingBox with: read at: 0@0) / n asFloat < 8
]

{ #category : 'testing' }
JpegTest >> test32bppLE63 [
	| form n read |
	form _ JpegTest lenaColor64 copy: (0@0 extent: 63@63).
	self assert: form depth = 32.
	form writeJPEGfileNamed: 'test.jpg'.
	read _ 'test.jpg' asFileEntry formContents.
	self assert: read depth = 32.
	self assert: read extent = (63@63).

	n _ form boundingBox width * form boundingBox height * 3.
	self assert: (form pixelCompare: form boundingBox with: read at: 0@0) / n asFloat < 8
]

{ #category : 'testing' }
JpegTest >> testBA16bpp [
	| form n read bytes |
	form _ JpegTest lenaColor64 asFormOfDepth: 16.
	self assert: form nativeDepth = 16.
	bytes _ JPEGReadWriter2 new compress: form quality: -1 progressiveJPEG: false.
	read _ Form extent: form extent depth: 16.
	JPEGReadWriter2 new uncompress: bytes into: read.

	n _ form boundingBox width * form boundingBox height * 3.
	self assert: (form pixelCompare: form boundingBox with: read at: 0@0) / n asFloat < 8
]

{ #category : 'testing' }
JpegTest >> testBA16bpp61 [
	| form n read bytes |
	form _ (JpegTest lenaColor64 asFormOfDepth: 16) copy: (0@0 extent: 61@61).
	self assert: form nativeDepth = 16.
	bytes _ JPEGReadWriter2 new compress: form quality: -1 progressiveJPEG: false.
	read _ Form extent: form extent depth: 16.
	JPEGReadWriter2 new uncompress: bytes into: read.

	n _ form boundingBox width * form boundingBox height * 3.
	self assert: (form pixelCompare: form boundingBox with: read at: 0@0) / n asFloat < 8
]

{ #category : 'testing' }
JpegTest >> testBA16bpp62 [
	| form n read bytes |
	form _ (JpegTest lenaColor64 asFormOfDepth: 16) copy: (0@0 extent: 62@62).
	self assert: form nativeDepth = 16.
	bytes _ JPEGReadWriter2 new compress: form quality: -1 progressiveJPEG: false.
	read _ Form extent: form extent depth: 16.
	JPEGReadWriter2 new uncompress: bytes into: read.

	n _ form boundingBox width * form boundingBox height * 3.
	self assert: (form pixelCompare: form boundingBox with: read at: 0@0) / n asFloat < 8
]

{ #category : 'testing' }
JpegTest >> testBA16bpp63 [
	| form n read bytes |
	form _ (JpegTest lenaColor64 asFormOfDepth: 16) copy: (0@0 extent: 63@63).
	self assert: form nativeDepth = 16.
	bytes _ JPEGReadWriter2 new compress: form quality: -1 progressiveJPEG: false.
	read _ Form extent: form extent depth: 16.
	JPEGReadWriter2 new uncompress: bytes into: read.

	n _ form boundingBox width * form boundingBox height * 3.
	self assert: (form pixelCompare: form boundingBox with: read at: 0@0) / n asFloat < 8
]

{ #category : 'testing' }
JpegTest >> testBA16bppLE [
	| form n read bytes |
	form _ JpegTest lenaColor64 asFormOfDepth: -16.
	self assert: form nativeDepth = -16.
	bytes _ JPEGReadWriter2 new compress: form quality: -1 progressiveJPEG: false.
	read _ Form extent: form extent depth: 16.
	JPEGReadWriter2 new uncompress: bytes into: read.

	n _ form boundingBox width * form boundingBox height * 3.
	self assert: (form pixelCompare: form boundingBox with: read at: 0@0) / n asFloat < 8
]

{ #category : 'testing' }
JpegTest >> testBA16bppLE61 [
	| form n read bytes |
	form _ (JpegTest lenaColor64 asFormOfDepth: -16) copy: (0@0 extent: 61@61).
	self assert: form nativeDepth = -16.
	bytes _ JPEGReadWriter2 new compress: form quality: -1 progressiveJPEG: false.
	read _ Form extent: form extent depth: 16.
	JPEGReadWriter2 new uncompress: bytes into: read.

	n _ form boundingBox width * form boundingBox height * 3.
	self assert: (form pixelCompare: form boundingBox with: read at: 0@0) / n asFloat < 8
]

{ #category : 'testing' }
JpegTest >> testBA16bppLE62 [
	| form n read bytes |
	form _ (JpegTest lenaColor64 asFormOfDepth: -16) copy: (0@0 extent: 62@62).
	self assert: form nativeDepth = -16.
	bytes _ JPEGReadWriter2 new compress: form quality: -1 progressiveJPEG: false.
	read _ Form extent: form extent depth: 16.
	JPEGReadWriter2 new uncompress: bytes into: read.

	n _ form boundingBox width * form boundingBox height * 3.
	self assert: (form pixelCompare: form boundingBox with: read at: 0@0) / n asFloat < 8
]

{ #category : 'testing' }
JpegTest >> testBA16bppLE63 [
	| form n read bytes |
	form _ (JpegTest lenaColor64 asFormOfDepth: -16) copy: (0@0 extent: 63@63).
	self assert: form nativeDepth = -16.
	bytes _ JPEGReadWriter2 new compress: form quality: -1 progressiveJPEG: false.
	read _ Form extent: form extent depth: 16.
	JPEGReadWriter2 new uncompress: bytes into: read.

	n _ form boundingBox width * form boundingBox height * 3.
	self assert: (form pixelCompare: form boundingBox with: read at: 0@0) / n asFloat < 8
]

{ #category : 'testing' }
JpegTest >> testBA32bpp [
	| form n read bytes |
	form _ JpegTest lenaColor64.
	self assert: form depth = 32.
	bytes _ JPEGReadWriter2 new compress: form quality: -1 progressiveJPEG: false.
	read _ JPEGReadWriter2 new uncompress: bytes into: nil.
	self assert: read depth = 32.

	n _ form boundingBox width * form boundingBox height * 3.
	self assert: (form pixelCompare: form boundingBox with: read at: 0@0) / n asFloat < 8
]

{ #category : 'testing' }
JpegTest >> testBA32bpp63 [
	| form n read bytes |
	form _ JpegTest lenaColor64 copy: (0@0 extent: 63@63).
	self assert: form depth = 32.
	bytes _ JPEGReadWriter2 new compress: form quality: -1 progressiveJPEG: false.
	read _ JPEGReadWriter2 new uncompress: bytes into: nil.
	self assert: read depth = 32.
	self assert: read extent = (63@63).

	n _ form boundingBox width * form boundingBox height * 3.
	self assert: (form pixelCompare: form boundingBox with: read at: 0@0) / n asFloat < 8
]

{ #category : 'testing' }
JpegTest >> testGray [
	| form n read |
	form _ JpegTest lenaColor64 asGrayForm.
	self assert: form nativeDepth = -8.
	form writeJPEGfileNamed: 'test.jpg'.
	read _ 'test.jpg' asFileEntry formContents.
	self assert: read nativeDepth = -8.

	n _ form boundingBox width * form boundingBox height.
	self assert: (form pixelCompare: form boundingBox with: read at: 0@0) / n asFloat < 8
]

{ #category : 'testing' }
JpegTest >> testGray61 [
	| form n read |
	form _ JpegTest lenaColor64 asGrayForm copy: (0@0 extent: 61@61).
	self assert: form nativeDepth = -8.
	form writeJPEGfileNamed: 'test.jpg'.
	read _ 'test.jpg' asFileEntry formContents.
	self assert: read nativeDepth = -8.
	self assert: read extent = (61@61).

	n _ form boundingBox width * form boundingBox height.
	self assert: (form pixelCompare: form boundingBox with: read at: 0@0) / n asFloat < 8
]

{ #category : 'testing' }
JpegTest >> testGray62 [
	| form n read |
	form _ JpegTest lenaColor64 asGrayForm copy: (0@0 extent: 62@62).
	self assert: form nativeDepth = -8.
	form writeJPEGfileNamed: 'test.jpg'.
	read _ 'test.jpg' asFileEntry formContents.
	self assert: read nativeDepth = -8.
	self assert: read extent = (62@62).

	n _ form boundingBox width * form boundingBox height.
	self assert: (form pixelCompare: form boundingBox with: read at: 0@0) / n asFloat < 8
]

{ #category : 'testing' }
JpegTest >> testGray63 [
	| form n read |
	form _ JpegTest lenaColor64 asGrayForm copy: (0@0 extent: 63@63).
	self assert: form nativeDepth = -8.
	form writeJPEGfileNamed: 'test.jpg'.
	read _ 'test.jpg' asFileEntry formContents.
	self assert: read nativeDepth = -8.
	self assert: read extent = (63@63).

	n _ form boundingBox width * form boundingBox height.
	self assert: (form pixelCompare: form boundingBox with: read at: 0@0) / n asFloat < 8
]

{ #category : 'testing' }
JpegTest >> testGrayBA [
	| form n read bytes |
	form _ JpegTest lenaColor64 asGrayForm.
	self assert: form nativeDepth = -8.
	bytes _ JPEGReadWriter2 new compress: form quality: -1 progressiveJPEG: false.
	read _ JPEGReadWriter2 new uncompress: bytes into: nil.
	self assert: read nativeDepth = -8.

	n _ form boundingBox width * form boundingBox height.
	self assert: (form pixelCompare: form boundingBox with: read at: 0@0) / n asFloat < 8
]

{ #category : 'testing' }
JpegTest >> testGrayBA61 [
	| form n read bytes |
	form _ JpegTest lenaColor64 asGrayForm copy: (0@0 extent: 61@61).
	self assert: form nativeDepth = -8.
	bytes _ JPEGReadWriter2 new compress: form quality: -1 progressiveJPEG: false.
	read _ JPEGReadWriter2 new uncompress: bytes into: nil.
	self assert: read nativeDepth = -8.
	self assert: read extent = (61@61).

	n _ form boundingBox width * form boundingBox height.
	self assert: (form pixelCompare: form boundingBox with: read at: 0@0) / n asFloat < 8
]

{ #category : 'testing' }
JpegTest >> testGrayBA62 [
	| form n read bytes |
	form _ JpegTest lenaColor64 asGrayForm copy: (0@0 extent: 62@62).
	self assert: form nativeDepth = -8.
	bytes _ JPEGReadWriter2 new compress: form quality: -1 progressiveJPEG: false.
	read _ JPEGReadWriter2 new uncompress: bytes into: nil.
	self assert: read nativeDepth = -8.
	self assert: read extent = (62@62).

	n _ form boundingBox width * form boundingBox height.
	self assert: (form pixelCompare: form boundingBox with: read at: 0@0) / n asFloat < 8
]

{ #category : 'testing' }
JpegTest >> testGrayBA63 [
	| form n read bytes |
	form _ JpegTest lenaColor64 asGrayForm copy: (0@0 extent: 63@63).
	self assert: form nativeDepth = -8.
	bytes _ JPEGReadWriter2 new compress: form quality: -1 progressiveJPEG: false.
	read _ JPEGReadWriter2 new uncompress: bytes into: nil.
	self assert: read nativeDepth = -8.
	self assert: read extent = (63@63).

	n _ form boundingBox width * form boundingBox height.
	self assert: (form pixelCompare: form boundingBox with: read at: 0@0) / n asFloat < 8
]
