def repeated(t, k):
    """Return the first value in iterator T that appears K times in a row.
    Iterate through the items such that if the same iterator is passed into
    the function twice, it continues in the second call at the point it left
    off in the first.

    >>> s = iter([10, 9, 10, 9, 9, 10, 8, 8, 8, 7])
    >>> repeated(s, 2)
    9
    >>> s2 = iter([10, 9, 10, 9, 9, 10, 8, 8, 8, 7])
    >>> repeated(s2, 3)
    8
    >>> s = iter([3, 2, 2, 2, 1, 2, 1, 4, 4, 5, 5, 5])
    >>> repeated(s, 3)
    2
    >>> repeated(s, 3)
    5
    >>> s2 = iter([4, 1, 6, 6, 7, 7, 8, 8, 2, 2, 2, 5])
    >>> repeated(s2, 3)
    2
    """
    assert k > 1
    prev=next(t)
    index=1
    try:
        while True:
            tmp=next(t)
            if prev==tmp:
                index+=1
                if index==k:
                    return tmp
            else:
                prev=tmp
                index=1
            
    except StopIteration:
        return


def merge(incr_a, incr_b):
    """Yield the elements of strictly increasing iterables incr_a and incr_b, removing
    repeats. Assume that incr_a and incr_b have no repeats. incr_a or incr_b may or may not
    be infinite sequences.

    >>> m = merge([0, 2, 4, 6, 8, 10, 12, 14], [0, 3, 6, 9, 12, 15])
    >>> type(m)
    <class 'generator'>
    >>> list(m)
    [0, 2, 3, 4, 6, 8, 9, 10, 12, 14, 15]
    >>> def big(n):
    ...    k = 0
    ...    while True: yield k; k += n
    >>> m = merge(big(2), big(3))
    >>> [next(m) for _ in range(11)]
    [0, 2, 3, 4, 6, 8, 9, 10, 12, 14, 15]
    """
    iter_a, iter_b = iter(incr_a), iter(incr_b)
    next_a, next_b = next(iter_a, None), next(iter_b, None)
    while next_a != None or next_b != None:
        if next_a == None:
            yield next_b
            next_b = next(iter_b,None)
        elif next_b == None:
            yield next_a
            next_a = next(iter_a,None)
        else:
            if next_a > next_b:
                yield next_b
                next_b = next(iter_b,None)
            elif next_b > next_a:
                yield next_a
                next_a = next(iter_a,None)
            else:
                yield next_a
                next_a = next(iter_a,None)
                next_b = next(iter_b,None)


def deep_len(lnk):
    """ Returns the deep length of a possibly deep linked list.

    >>> deep_len(Link(1, Link(2, Link(3))))
    3
    >>> deep_len(Link(Link(1, Link(2)), Link(3, Link(4))))
    4
    >>> levels = Link(Link(Link(1, Link(2)), \
            Link(3)), Link(Link(4), Link(5)))
    >>> print(levels)
    <<<1 2> 3> <4> 5>
    >>> deep_len(levels)
    5
    """
    if lnk == Link.empty:
        return 0
    elif type(lnk) != Link or lnk.rest == Link.empty:
        return 1
    else:
        return deep_len(lnk.first) + deep_len(lnk.rest)


def add_d_leaves(t, v):
    """Add d leaves containing v to each node at every depth d.

    >>> t_one_to_four = Tree(1, [Tree(2), Tree(3, [Tree(4)])])
    >>> print(t_one_to_four)
    1
      2
      3
        4
    >>> add_d_leaves(t_one_to_four, 5)
    >>> print(t_one_to_four)
    1
      2
        5
      3
        4
          5
          5
        5

    >>> t1 = Tree(1, [Tree(3)])
    >>> add_d_leaves(t1, 4)
    >>> t1
    Tree(1, [Tree(3, [Tree(4)])])
    >>> t2 = Tree(2, [Tree(5), Tree(6)])
    >>> t3 = Tree(3, [t1, Tree(0), t2])
    >>> print(t3)
    3
      1
        3
          4
      0
      2
        5
        6
    >>> add_d_leaves(t3, 10)
    >>> print(t3)
    3
      1
        3
          4
            10
            10
            10
          10
          10
        10
      0
        10
      2
        5
          10
          10
        6
          10
          10
        10
    """
    depth = 0
    def add_d(t, depth):
        for b in t.branches:
            add_d(b, depth+1)
        for i in range(depth):
            t.branches.append(Tree(v))
    add_d(t, depth)


def insert_into_all(item, nested_list):
    """Return a new list consisting of all the lists in nested_list,
    but with item added to the front of each. You can assume that
     nested_list is a list of lists.

    >>> nl = [[], [1, 2], [3]]
    >>> insert_into_all(0, nl)
    [[0], [0, 1, 2], [0, 3]]
    """
    return [[item] + lst for lst in nested_list]


def subseqs(s:list):
    """Return a nested list (a list of lists) of all subsequences of S.
    The subsequences can appear in any order. You can assume S is a list.

    >>> seqs = subseqs([1, 2, 3])
    >>> sorted(seqs)
    [[], [1], [1, 2], [1, 2, 3], [1, 3], [2], [2, 3], [3]]
    >>> subseqs([])
    [[]]
    """
    if s == []:
        return [[]]
    else:
        sublst = subseqs(s[1:])
        return insert_into_all(s[0],sublst) + sublst  # 将子序列分为含有第一个元素的和不含有第一个元素的

        

def non_decrease_subseqs(s):
    """Assuming that S is a list, return a nested list of all subsequences
    of S (a list of lists) for which the elements of the subsequence
    are strictly nondecreasing. The subsequences can appear in any order.

    >>> seqs = non_decrease_subseqs([1, 3, 2])
    >>> sorted(seqs)
    [[], [1], [1, 2], [1, 3], [2], [3]]
    >>> non_decrease_subseqs([])
    [[]]
    >>> seqs2 = non_decrease_subseqs([1, 1, 2])
    >>> sorted(seqs2)
    [[], [1], [1], [1, 1], [1, 1, 2], [1, 2], [1, 2], [2]]
    """
    def subseq_helper(s, prev):
        if not s:
            return [[]]
        elif s[0] < prev:
            return [[prev]] + subseq_helper(s, s[0])
        else:
            a = subseq_helper(s[1:], s[1] if s[1:] != [] else s[0])
            b = [lst for lst in a if not lst or lst[0] >= s[0] ]
            return insert_into_all(s[0], b) + a
    return subseq_helper(s, s[0] if s != [] else None)


def card(n):
    """Return the playing card numeral as a string for a positive n <= 13."""
    assert type(n) == int and n > 0 and n <= 13, "Bad card n"
    specials = {1: 'A', 11: 'J', 12: 'Q', 13: 'K'}
    return specials.get(n, str(n))


def shuffle(cards):
    """Return a shuffled list that interleaves the two halves of cards.

    >>> shuffle(range(6))
    [0, 3, 1, 4, 2, 5]
    >>> suits = ['H', 'D', 'S', 'C']
    >>> cards = [card(n) + suit for n in range(1,14) for suit in suits]
    >>> cards[:12]
    ['AH', 'AD', 'AS', 'AC', '2H', '2D', '2S', '2C', '3H', '3D', '3S', '3C']
    >>> cards[26:30]
    ['7S', '7C', '8H', '8D']
    >>> shuffle(cards)[:12]
    ['AH', '7S', 'AD', '7C', 'AS', '8H', 'AC', '8D', '2H', '8S', '2D', '8C']
    >>> shuffle(shuffle(cards))[:12]
    ['AH', '4D', '7S', '10C', 'AD', '4S', '7C', 'JH', 'AS', '4C', '8H', 'JD']
    >>> cards[:12]  # Should not be changed
    ['AH', 'AD', 'AS', 'AC', '2H', '2D', '2S', '2C', '3H', '3D', '3S', '3C']
    """
    assert len(cards) % 2 == 0, 'len(cards) must be even'
    half = cards[len(cards)//2:]
    shuffled = []
    for i in range(0, len(cards)//2):
        shuffled.append(cards[i])
        shuffled.append(half[i])
    return shuffled


def pairs(lst):
    """
    >>> type(pairs([3, 4, 5]))
    <class 'generator'>
    >>> for x, y in pairs([3, 4, 5]):
    ...     print(x, y)
    ...
    3 3
    3 4
    3 5
    4 3
    4 4
    4 5
    5 3
    5 4
    5 5
    """
    for i in lst:
        for j in lst:
            yield i, j


class PairsIterator:
    """
    >>> for x, y in PairsIterator([3, 4, 5]):
    ...     print(x, y)
    ...
    3 3
    3 4
    3 5
    4 3
    4 4
    4 5
    5 3
    5 4
    5 5
    """

    def __init__(self, lst):
        assert len(lst) > 0
        self.lst=lst[:]
        self.index1=0
        self.index2=-1
        self.leng = len(self.lst)


    def __next__(self):
        if self.index1 >= self.leng - 1 and self.index2 >= self.leng - 1:
            raise StopIteration
        if self.index2 >= self.leng - 1:
            self.index2 = 0
            self.index1 += 1
        else:
            self.index2 += 1
        return self.lst[self.index1], self.lst[self.index2]

    def __iter__(self):
        return self


def long_paths(tree, n):
    """Return a list of all paths in tree with length at least n.

    >>> t = Tree(3, [Tree(4), Tree(4), Tree(5)])
    >>> left = Tree(1, [Tree(2), t])
    >>> mid = Tree(6, [Tree(7, [Tree(8)]), Tree(9)])
    >>> right = Tree(11, [Tree(12, [Tree(13, [Tree(14)])])])
    >>> whole = Tree(0, [left, Tree(13), mid, right])
    >>> for path in long_paths(whole, 2):
    ...     print(path)
    ...
    <0 1 2>
    <0 1 3 4>
    <0 1 3 4>
    <0 1 3 5>
    <0 6 7 8>
    <0 6 9>
    <0 11 12 13 14>
    >>> for path in long_paths(whole, 3):
    ...     print(path)
    ...
    <0 1 3 4>
    <0 1 3 4>
    <0 1 3 5>
    <0 6 7 8>
    <0 11 12 13 14>
    >>> long_paths(whole, 4)
    [Link(0, Link(11, Link(12, Link(13, Link(14)))))]
    """
    res=[]
    def n_path(t, depth, lnk):
        if depth >= n and not t.branches:
            lnk.append(t.label)
            tmp=Link.empty
            for i in lnk[::-1]:
                tmp=Link(i, tmp)
            res.append(tmp)
            lnk.pop()
        for b in t.branches:
            lnk.append(t.label)
            n_path(b, depth+1, lnk)
            lnk.pop()
    n_path(tree, 0, [])
    return res


def flip_two(s):
    """
    >>> one_lnk = Link(1)
    >>> flip_two(one_lnk)
    >>> one_lnk
    Link(1)
    >>> lnk = Link(1, Link(2, Link(3, Link(4, Link(5)))))
    >>> flip_two(lnk)
    >>> lnk
    Link(2, Link(1, Link(4, Link(3, Link(5)))))
    """
    lnk_first = s
    lnk_sec = s.rest
    if lnk_first is not Link.empty and lnk_sec is not Link.empty:
        tmp = lnk_first.first
        lnk_first.first = lnk_sec.first
        lnk_sec.first = tmp
        flip_two(s.rest.rest)


    # For an extra challenge, try writing out an iterative approach as well below!
    # lnk_first = s
    # lnk_sec = s.rest
    # while lnk_first is not Link.empty and lnk_sec is not Link.empty:
    #     tmp = lnk_first.first
    #     lnk_first.first = lnk_sec.first
    #     lnk_sec.first = tmp
    #     lnk_first = lnk_first.rest.rest
    #     lnk_sec = lnk_sec.rest.rest


class Link:
    """A linked list.

    >>> s = Link(1)
    >>> s.first
    1
    >>> s.rest is Link.empty
    True
    >>> s = Link(2, Link(3, Link(4)))
    >>> s.first = 5
    >>> s.rest.first = 6
    >>> s.rest.rest = Link.empty
    >>> s                                    # Displays the contents of repr(s)
    Link(5, Link(6))
    >>> s.rest = Link(7, Link(Link(8, Link(9))))
    >>> s
    Link(5, Link(7, Link(Link(8, Link(9)))))
    >>> print(s)                             # Prints str(s)
    <5 7 <8 9>>
    """
    empty = ()

    def __init__(self, first, rest=empty):
        assert rest is Link.empty or isinstance(rest, Link)
        self.first = first
        self.rest = rest

    def __repr__(self):
        if self.rest is not Link.empty:
            rest_repr = ', ' + repr(self.rest)
        else:
            rest_repr = ''
        return 'Link(' + repr(self.first) + rest_repr + ')'

    def __str__(self):
        string = '<'
        while self.rest is not Link.empty:
            string += str(self.first) + ' '
            self = self.rest
        return string + str(self.first) + '>'


class Tree:
    """
    >>> t = Tree(3, [Tree(2, [Tree(5)]), Tree(4)])
    >>> t.label
    3
    >>> t.branches[0].label
    2
    >>> t.branches[1].is_leaf()
    True
    """

    def __init__(self, label, branches=[]):
        for b in branches:
            assert isinstance(b, Tree)
        self.label = label
        self.branches = list(branches)

    def is_leaf(self):
        return not self.branches

    def __repr__(self):
        if self.branches:
            branch_str = ', ' + repr(self.branches)
        else:
            branch_str = ''
        return 'Tree({0}{1})'.format(self.label, branch_str)

    def __str__(self):
        def print_tree(t, indent=0):
            tree_str = '  ' * indent + str(t.label) + "\n"
            for b in t.branches:
                tree_str += print_tree(b, indent + 1)
            return tree_str
        return print_tree(self).rstrip()
