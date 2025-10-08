import pandas as pd
import pickle
import os
from functools import lru_cache


def read_excel(file_path, sheet_name=0):
    cache_file = f"{file_path}_{sheet_name}.pkl"

    if os.path.exists(cache_file):
        print("Loading from cache...")
        with open(cache_file, "rb") as f:
            return pickle.load(f)

    print("Reading Excel file...")
    df = pd.read_excel(file_path, sheet_name=sheet_name)

    with open(cache_file, "wb") as f:
        pickle.dump(df, f)

    return df


def text_cleaning(df):
    """Clean occupation text - lowercase, remove punctuation and stop words."""
    from nltk.corpus import stopwords
    import nltk

    nltk.download("stopwords", quiet=True)
    stop_words = set(stopwords.words("english"))

    def remove_stopwords(text):
        if pd.isna(text):
            return text
        words = text.split()
        # Remove stop words and single character words (including numbers)
        return " ".join([word for word in words if word not in stop_words])

    df = df.dropna(subset=["OCCUPATION_DESC1"])
    df = df.copy()

    # Phrase mapping
    mapping = {
        "g police": "government police",
        "doctor g p and specialist": "doctor general practitioner",
        "doctor g p": "doctor general practitioner",
        "supervisor c g": "supervisor",
        "assistant admin officer p h": "assistant admin officer",
        "x ray assistant": "xray assistant",
        "x ray attendant": "xray attendant",
        "artisan pipe fitter w u c": "artisan pipe fitter water",
        "technical officer c": "technical officer",
        "c w artisan railway palapye": "civil works artisan railway palapye",
        "f w educator": "family wealth educator",
        "family w educator": "family wealth educator",
        "technical assistant agriculture cp f": "technical assistant agriculture",
        "f b controller": "food beverage controller",
        "security supervisor b c v c": "security supervisor",
        "loco driver b c l": "mining train driver",
        "b c l shift boss": "mining shift boss",
        "l h operator": "load haul operator",
        "seam stress h sewing": "seam stress sewing",
        "examine tittle deeds" : "examiner title deeds",
        "ramotswa" : "",
        "mainline botswana railwa" : "",
        "non formal" : "",
        "tecnical" : "technical",
        "gen" : "general",
    }

    def expand_phrases(text):
        for phrase, expansion in sorted(mapping.items(), key=lambda x: len(x[0]), reverse=True):
            if phrase in text:
                text = text.replace(phrase, expansion)
        return text

    df["OCCUPATION_DESC1"] = (
        df["OCCUPATION_DESC1"]
        .str.lower()
        .str.strip()
        .str.replace(r"[^\w\s]", " ", regex=True)
        .str.replace(r"\d+", " ", regex=True)  # Remove all digits
        .str.replace(r"\s+", " ", regex=True)
        .apply(remove_stopwords)
        .apply(expand_phrases)
    )
    # Remove rows with missing or empty occupation descriptions
    
    df = df[df["OCCUPATION_DESC1"].str.strip() != ""]
    return df


def fill_missing_codes(df):
    """Fill missing occupation codes using most frequent code per description."""
    most_freq_codes = (
        df[df["OCCUPATION_CODE"].notna()]
        .groupby(["OCCUPATION_DESC1", "OCCUPATION_CODE"])
        .size()
        .reset_index(name="freq")
        .sort_values(["OCCUPATION_DESC1", "freq"], ascending=[True, False])
        .drop_duplicates(subset=["OCCUPATION_DESC1"], keep="first")[
            ["OCCUPATION_DESC1", "OCCUPATION_CODE"]
        ]
    )

    df_filled = df.merge(
        most_freq_codes, on="OCCUPATION_DESC1", how="left", suffixes=("", "_mostfreq")
    )

    df_filled["OCCUPATION_CODE"] = df_filled["OCCUPATION_CODE"].fillna(
        df_filled["OCCUPATION_CODE_mostfreq"]
    )
    df_filled = df_filled.drop(columns=["OCCUPATION_CODE_mostfreq"], errors="ignore")
    return df_filled


def fill_missing_descriptions(df):
    """Fill missing occupation descriptions using most frequent description per code."""
    most_freq_desc = (
        df[df["OCCUPATION_DESC1"].notna()]
        .groupby(["OCCUPATION_CODE", "OCCUPATION_DESC1"])
        .size()
        .reset_index(name="freq")
        .sort_values(["OCCUPATION_CODE", "freq"], ascending=[True, False])
        .drop_duplicates(subset=["OCCUPATION_CODE"], keep="first")[
            ["OCCUPATION_CODE", "OCCUPATION_DESC1"]
        ]
    )

    df_filled = df.merge(
        most_freq_desc, on="OCCUPATION_CODE", how="left", suffixes=("", "_mostfreq")
    )

    df_filled["OCCUPATION_DESC1"] = df_filled["OCCUPATION_DESC1"].fillna(
        df_filled["OCCUPATION_DESC1_mostfreq"]
    )
    df_filled = df_filled.drop(columns=["OCCUPATION_DESC1_mostfreq"], errors="ignore")
    return df_filled


def tokenise(df):
    """Return all words from OCCUPATION_DESC1 column as a list."""
    all_words = []
    for text in df["OCCUPATION_DESC1"].dropna():
        all_words.extend(text.split())
    return all_words


def tokenise_with_context(df):
    """Return words and mapping of word to original phrases."""
    all_words = []
    word_to_phrases = {}

    for text in df["OCCUPATION_DESC1"].dropna():
        words = text.split()
        all_words.extend(words)

        for word in words:
            if word not in word_to_phrases:
                word_to_phrases[word] = set()
            word_to_phrases[word].add(text)

    return all_words, word_to_phrases


def word_counts(word_list):
    """Return dictionary mapping words to their counts."""
    from collections import Counter

    return dict(Counter(word_list))


def levenshtein_distances(target_word, word_list):
    """Calculate Levenshtein distance from target_word to all words in word_list."""
    import Levenshtein

    return {word: Levenshtein.distance(target_word, word) for word in word_list}


def spell_check(word, word_counts_dict, max_distance=2, return_candidates=False):
    """Spell check with confidence based on frequency and distance."""
    import Levenshtein

    # Domain-relevant words for occupations
    occupation_words = {
        "system",
        "systems",
        "computer",
        "technical",
        "engineer",
        "manager",
        "officer",
        "assistant",
        "operator",
        "technician",
        "radio",
        "telephone",
        "communications",
        "mechanical",
        "electrical",
        "construction",
        "maintenance",
        "supervisor",
        "inspector",
        "analyst",
    }

    # If word exists, return it with high confidence
    if word in word_counts_dict:
        if return_candidates:
            return word, 1.0, []
        return word, 1.0

    candidates = []
    for candidate, freq in word_counts_dict.items():
        distance = Levenshtein.distance(word, candidate)
        if distance <= max_distance:
            # Confidence based on distance and frequency
            distance_score = 1 - (distance / max(len(word), len(candidate)))
            freq_score = min(freq / 100, 1.0)  # Normalize frequency

            # Domain bonus for occupation-related words
            domain_bonus = 0.1 if candidate in occupation_words else 0

            confidence = (distance_score * 0.7) + (freq_score * 0.3) + domain_bonus
            candidates.append((candidate, confidence, distance, freq))

    if not candidates:
        if return_candidates:
            return word, 0.0, []
        return word, 0.0  # No suggestions

    # Sort by confidence
    candidates.sort(key=lambda x: x[1], reverse=True)
    best = candidates[0]

    if return_candidates:
        top_5 = candidates[:5]
        return best[0], best[1], top_5

    return best[0], best[1]


if __name__ == "__main__":
    print("READING IN DATA")
    df = read_excel(
        "/Users/richardlastrucci/Documents/university/msc/trg 880/capstone/trg-880-capstone-2025/data/SUMMARY12025.xlsx"
    )
    print("READ IN DATA")

    print("FILLING MISSING CODES")
    df = fill_missing_codes(df)

    print("FILLING MISSING DESCRIPTIONS")
    df = fill_missing_descriptions(df)

    print("CLEANING TEXT")
    df = text_cleaning(df)


    print("\nOCCUPATION_CODE head:")
    print(df["OCCUPATION_DESC1"].head())

    print("\nTOKENISING")
    words, word_to_phrases = tokenise_with_context(df)
    print(f"Total words: {len(words)}")
    print(f"First 10 words: {words[:10]}")

    print("\nCOUNTING WORDS")
    counts = word_counts(words)
    print(f"Total unique words (dictionary keys): {len(counts)}")

    # Find single character words
    single_char_words = {
        word: count for word, count in counts.items() if len(word) == 1
    }
    print(f"\nSingle character words: {len(single_char_words)}")
    
    top_10 = sorted(counts.items(), key=lambda x: x[1], reverse=True)[:10]

    print(f"Top 10 most frequent: {top_10}")
    bottom_10 = sorted(counts.items(), key=lambda x: x[1])[:10]
    print(f"Top 10 least frequent: {bottom_10}")
    unique_count = sum(1 for count in counts.values() if count == 1)
    print(f"Words appearing only once: {unique_count}")

    print("\nLEVENSHTEIN DISTANCE DEMO")
    unique_words = list(counts.keys())[:50]  # Use first 50 unique words for demo
    target_word = unique_words[0]
    distances = levenshtein_distances(target_word, unique_words)
    closest_5 = sorted(distances.items(), key=lambda x: x[1])[:5]
    print(f"Target word: '{target_word}'")
    print(f"5 closest words: {closest_5}")

    print("\nSPELL CHECK DEMO")
    test_words = ["managr", "teachr", "drivr", "clrk", "nurs"]  # Common misspellings
    for test_word in test_words:
        correction, confidence = spell_check(test_word, counts)
        print(f"'{test_word}' -> '{correction}' (confidence: {confidence:.3f})")

    print("\nRUNNING SPELL CHECK ON LOW FREQUENCY WORDS")
    # Separate high and low frequency words
    min_freq = 3
    high_freq_words = {word: freq for word, freq in counts.items() if freq >= min_freq}
    low_freq_words = {word: freq for word, freq in counts.items() if freq < min_freq}

    print(
        f"Checking {len(low_freq_words)} low-frequency words against {len(high_freq_words)} high-frequency words"
    )

    corrections = []
    for word in low_freq_words:
        corrected, confidence, candidates = spell_check(
            word, high_freq_words, return_candidates=True
        )
        corrected_freq = counts.get(corrected, 0)
        if (
            word != corrected
            and confidence > 0.5
            and corrected_freq >= 5
            and len(corrected) > 1
        ):  # Only save good corrections with freq >= 5 and avoid single chars
            # Format candidates as string
            candidates_str = "; ".join([f"{c[0]}({c[1]:.3f})" for c in candidates])
            # Get sample phrases containing this word
            sample_phrases = list(word_to_phrases.get(word, []))[:3]  # First 3 phrases
            phrases_str = "; ".join(sample_phrases)

            corrections.append(
                {
                    "original": word,
                    "corrected": corrected,
                    "confidence": confidence,
                    "original_freq": counts[word],
                    "corrected_freq": corrected_freq,
                    "top_candidates": candidates_str,
                    "sample_phrases": phrases_str,
                }
            )

    corrections_df = pd.DataFrame(corrections)
    corrections_df.to_csv("spell_corrections.csv", index=False)
    print(f"Saved {len(corrections)} corrections to spell_corrections.csv")
