/* message "Hello Progress!" view-as alert-box info buttons ok. */
define variable str as char.

message 'Möchten Sie wirklich ...?'
	view-as alert-box question
	buttons yes-no-cancel
	set lRueckgabe as logical.

str = "hello, world".
str = string(lRueckgabe) + str.
display str.