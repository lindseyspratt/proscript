# Library
The files in the library directory have common useful predicates.
Some of these files are from the Edinburgh DEC-10 Prolog as found 
at http://www.j-paine.org/prolog/library.html

## between.pl
From Edinburgh DEC-10 library.
* between(+L, +U, ?N): True for all integers N between L (lower) and U (upper).
If N is unbound on evaluation then it is generated in order from L to U (repeating on failure).

* gen_arg(?N, +Term, ?Arg):
is exactly like arg(N, Term, Arg), except that it will generate
solutions for N by backtracking (will work when N is a variable).

### gen_nat/1, gen_nat/2, gen_int/1: generate integers. 
Originally by R O'Keefe.

* gen_nat(+X): True if X is a natural number, false otherwise
* gen_nat(-X): Instantiates X to 0, then 1, 2, 3, 4...
* gen_nat(+L,-N): Instantiates N to L, then L+1, L+2...
* gen_nat(+L,+N): True if N >= L
* gen_nat(-L,+N):  ** Succeeds with L = N , then LOOPS **

* gen_int(-X)	 Instantiates X to 0, then 1, -1, 2, -2, 3, -3...

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
