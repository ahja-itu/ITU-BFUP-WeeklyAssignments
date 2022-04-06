module MultiSet
    type MultiSet<'a when 'a : comparison> = MS of Map<'a, uint32>
