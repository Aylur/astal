namespace AstalApps {
int max(int i1, int i2) { return i1 > i2 ? i1 : i2; }

int min(int i1, int i2) { return i1 > i2 ? i2 : i1; }

int min3(int a, int b, int c) {
    return (a < b) ? ((a < c) ? a : c) : ((b < c) ? b : c);
}

double jaro_winkler_similarity(string s1, string s2) {
    int len1 = s1.length;
    int len2 = s2.length;

    if (len1 == 0 && len2 == 0)
        return 1.0;
    if (len1 == 0 || len2 == 0)
        return 0.0;

    int match_distance = (max(len1, len2) / 2) - 1;
    bool[] s1_matches = new bool[len1];
    bool[] s2_matches = new bool[len2];

    int matches = 0;
    int transpositions = 0;

    for (int i = 0; i < len1; i++) {
        int start = max(0, i - match_distance);
        int end = min(i + match_distance + 1, len2);
        for (int j = start; j < end; j++) {
            if (s2_matches[j])
                continue;
            if (s1[i] != s2[j])
                continue;
            s1_matches[i] = true;
            s2_matches[j] = true;
            matches++;
            break;
        }
    }

    if (matches == 0)
        return 0.0;

    int k = 0;
    for (int i = 0; i < len1; i++) {
        if (!s1_matches[i])
            continue;
        while (!s2_matches[k])
            k++;
        if (s1[i] != s2[k])
            transpositions++;
        k++;
    }

    double m = matches;
    double jaro =
        ((m / len1) + (m / len2) + ((m - transpositions / 2.0) / m)) / 3.0;

    int prefix = 0;
    for (int i = 0; i < min(len1, len2); i++) {
        if (s1[i] == s2[i])
            prefix++;
        else
            break;
    }

    double jaro_winkler = jaro + (prefix * 0.1 * (1.0 - jaro));
    return jaro_winkler;
}

double levenshtein_distance(string s1, string s2) {
    int len1 = s1.length;
    int len2 = s2.length;
    int[, ] d = new int[len1 + 1, len2 + 1];

    for (int i = 0; i <= len1; i++) {
        d[i, 0] = i;
    }
    for (int j = 0; j <= len2; j++) {
        d[0, j] = j;
    }

    for (int i = 1; i <= len1; i++) {
        for (int j = 1; j <= len2; j++) {
            int cost = (s1[i - 1] == s2[j - 1]) ? 0 : 1;
            d[i, j] = min3(
                d[i - 1, j] + 1,       // deletion
                d[i, j - 1] + 1,       // insertion
                d[i - 1, j - 1] + cost // substitution
            );
        }
    }

    var distance = d[len1, len2];
    int max_len = max(s1.length, s2.length);

    if (max_len == 0) {
        return 1.0;
    }

    return 1.0 - ((double)distance / max_len);
}
}
