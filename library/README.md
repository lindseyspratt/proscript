# Library
The files in the library directory have common useful predicates.
Some of these files are from the Edinburgh DEC-10 Prolog as found 
at http://www.j-paine.org/prolog/library.html

## dom.pl
set_dom_name_path_value/2: set the value associated with HTML nodes that 
satisfy the given node name path. A path is a list of names where each name
is a list of character codes. The nodes on the path may not be sequential
(i.e. have a direct parent-child relationship).

## listut.pl
From Edinburgh DEC-10 library.

* correspond/4
* delete/3
* last/2
* nextto/3
* nmember/3
* nmembers/3
* nth0/3
* nth0/4
* nth1/3
* nth1/4
* numlist/3
* perm/2
* perm2/4
* remove_dups/2
* rev/2
* reverse/2
* same_length/2
* select/4
* shorter_list/2
* subseq/3
* subseq0/2
* subseq1/2
* sumlist/2

## listut2.pl
Additional list utility predicates created for Proscript.
* select_list/4
* contains_list/2
* split_list/4
* lowercase/2
* uppercase/2

## setutl.pl
From Edinburgh DEC-10 library.

* add_element/3
* del_element/3
* disjoint/1
* disjoint/2
* intersect/2
* intersect/3
* listtoset/2
* memberchk/2
* nonmember/2
* pairfrom/4
* select/3
* seteq/2
* subset/2
* subtract/3
* symdiff/3
* union/3
