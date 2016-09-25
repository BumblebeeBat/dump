bits:top2 and bits:height2 need to be updated. They are currently O(N) where N is the distance we need to do. We can upgrade them to be O(log2(N)) by pattern matching on the input as a binary search.

bits:ider could be memoized, but it is a minor upgrade.