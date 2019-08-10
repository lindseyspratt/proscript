convert_URL_to_base(URL, BaseURL) :-
  absolute_url(URL)
    -> URL = BaseURL
  ;
  current_compile_url(ReferenceURL)
    -> url_directory(ReferenceURL, ReferenceDirectory),
       resolve_url(ReferenceDirectory, URL, BaseURL)
  ;
  absolute_file_name(URL, Absolute, [])
    -> BaseURL = Absolute.

absolute_url(URL) :-
  atom_codes(URL, URLCodes),
  (append(":", _, Suffix),
   append(_, Suffix, URLCodes)
    -> true
  ;
  append("/", _, URLCodes)
  ).

url_directory(URL, Directory) :-
  atom_codes(URL, URLCodes),
  append("/", _, TestSuffix),
  append("/", FileCodes, Suffix),
  (append(Prefix, Suffix, URLCodes),
  \+ append(_, TestSuffix, FileCodes)
    -> append(Prefix, "/", DirectoryCodes),
       atom_codes(Directory, DirectoryCodes)
  ;
  Directory = './'
  ).


resolve_url('./', URL, URL) :- !.


resolve_url(Directory, URL, BaseURL) :-
  atom_codes(URL, URLCodes),
  sub(Directory, URLCodes, BaseURL).

sub(Directory, URLCodes, BaseURL) :-
  append("../", PrefixCodes, URLCodes)
    -> trim_directory(Directory, TrimmedDirectory),
       atom_codes(Prefix, PrefixCodes),
       resolve_url(TrimmedDirectory, Prefix, BaseURL)
   ;
   atom_codes(Directory, DirectoryCodes),
   append(DirectoryCodes, URLCodes, BaseURLCodes),
   atom_codes(BaseURL, BaseURLCodes).


% TrimmedDirectory is Directory with the last component
% removed. Directory = 'X/Y/' -> TrimmedDirectory = 'X/'.

trim_directory(Directory, TrimmedDirectory) :-
  atom_codes(Directory, DirectoryCodes),
  reverse(DirectoryCodes, ReversedCodes), % '/_RY/RX'
  append("/", RA, ReversedCodes), % RA = '_RY/RX'
  append(_RY, "/", RB),              % RB = _RY/
  append(RB, RX, RA),
  append("/", RX, RC),              % RC = /RX
  reverse(RC, C),
  atom_codes(TrimmedDirectory, C).
