
namespace AstalApps {

private int max(int a, int b) {
    return a > b ? a : b;
}

public int fuzzy_match_string(string pattern, string str) {
    const int unmatched_letter_penalty = -1;
    int score = 100;

    if (pattern.length == 0) return score;
    if (str.length < pattern.length) return int.MIN;

    score += unmatched_letter_penalty * (str.length - pattern.length);
    score = fuzzy_match_recurse(pattern, str, score, true);

    return score;
}

private int fuzzy_match_recurse(string pattern, string str, int score, bool first_char) {
    if (pattern.length == 0) return score;

    int match_idx = 0;
    int offset = 0;
    unichar search = pattern.casefold().get_char(0);
    int best_score = int.MIN;

    while ((match_idx = str.casefold().substring(offset).index_of_char(search)) >= 0) {
        offset += match_idx;
        int subscore = fuzzy_match_recurse(
            pattern.substring(1),
            str.substring(offset + 1),
            compute_score(offset, first_char, str, offset), false);
        best_score = max(best_score, subscore);
        offset++;
    }

    if (best_score == int.MIN) return int.MIN;
    return score + best_score;
}

private int compute_score(int jump, bool first_char, string match, int idx) {
    const int adjacency_bonus = 15;
    const int separator_bonus = 30;
    const int camel_bonus = 30;
    const int first_letter_bonus = 15;
    const int leading_letter_penalty = -5;
    const int max_leading_letter_penalty = -15;

    int score = 0;

    if (!first_char && jump == 0) {
        score += adjacency_bonus;
    }
    if (!first_char || jump > 0) {
        if (match[idx].isupper() && match[idx-1].islower()) {
            score += camel_bonus;
        }
        if (match[idx].isalnum() && !match[idx-1].isalnum()) {
            score += separator_bonus;
        }
    }
    if (first_char && jump == 0) {
        score += first_letter_bonus;
    }
    if (first_char) {
        score += max(leading_letter_penalty * jump, max_leading_letter_penalty);
    }

    return score;
}
}
