record Album where
  constructor MkAlbum
  artist: String
  title: String
  year: Integer

help: Album
help = MkAlbum "The Beatles" "Help" 1965

heroes: Album
heroes = MkAlbum "David Bowie" "Heroes" 1977

collection: List Album
collection = [heroes, help]

Eq Album where
  (MkAlbum artist title year) == (MkAlbum artist' title' year') = artist == artist' && title == title' && year == year'

Ord Album where
  compare (MkAlbum artist title year) (MkAlbum artist' title' year') = case compare artist artist' of
                                                                            EQ => case compare year year' of
                                                                                        EQ => compare title title'
                                                                                        year_comparison => year_comparison
                                                                            artist_comparison => artist_comparison

Show Album where
  show (MkAlbum artist title year) = title ++ " by " ++ artist ++ " (released " ++ show year ++ ")"
