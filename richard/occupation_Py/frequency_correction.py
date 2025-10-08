import pandas as pd
from collections import Counter
import Levenshtein
from tqdm import tqdm


def frequency_based_correction(df, column='OCCUPATION_DESC1_temp', 
                             min_frequency=5, max_distance=2):
    """
    Correct low-frequency words using Levenshtein distance to high-frequency words.
    
    Args:
        df: DataFrame with occupation data
        column: Column containing occupation descriptions
        min_frequency: Words below this frequency are candidates for correction
        max_distance: Maximum Levenshtein distance for corrections
    """
    
    # Tokenize and count word frequencies
    all_words = []
    for text in df[column].dropna():
        words = text.lower().split()
        all_words.extend(words)
    
    word_counts = Counter(all_words)
    print(f"Total unique words: {len(word_counts)}")
    
    # Separate high and low frequency words
    high_freq_words = {word for word, count in word_counts.items() if count >= min_frequency}
    low_freq_words = {word for word, count in word_counts.items() if count < min_frequency}
    
    print(f"High frequency words (>={min_frequency}): {len(high_freq_words)}")
    print(f"Low frequency words (<{min_frequency}): {len(low_freq_words)}")
    
    # Find corrections for low frequency words
    corrections = {}
    
    for low_word in tqdm(low_freq_words, desc="Finding corrections"):
        best_match = None
        best_distance = float('inf')
        
        for high_word in high_freq_words:
            distance = Levenshtein.distance(low_word, high_word)
            
            if distance <= max_distance and distance < best_distance:
                best_distance = distance
                best_match = high_word
        
        if best_match:
            corrections[low_word] = best_match
    
    print(f"Found {len(corrections)} potential corrections")
    
    # Save corrections to file
    corrections_df = pd.DataFrame([
        {
            'original_word': wrong,
            'corrected_word': correct,
            'original_frequency': word_counts[wrong],
            'corrected_frequency': word_counts[correct],
            'levenshtein_distance': Levenshtein.distance(wrong, correct)
        }
        for wrong, correct in corrections.items()
    ])
    
    corrections_df.to_csv('word_corrections.csv', index=False)
    print(f"Corrections saved to word_corrections.csv")
    
    # Show sample corrections for review
    print("\nSample corrections:")
    for i, (wrong, correct) in enumerate(list(corrections.items())[:10]):
        print(f"  {wrong} -> {correct} (freq: {word_counts[wrong]} -> {word_counts[correct]})")
    
    return corrections


def apply_corrections(df, corrections, column='OCCUPATION_DESC1_temp'):
    """Apply the corrections to the dataframe."""
    
    def correct_text(text):
        if pd.isna(text):
            return text
        
        words = text.split()
        corrected_words = []
        
        for word in words:
            corrected_word = corrections.get(word.lower(), word)
            corrected_words.append(corrected_word)
        
        return ' '.join(corrected_words)
    
    df_corrected = df.copy()
    df_corrected[f'{column}_corrected'] = df_corrected[column].apply(correct_text)
    
    return df_corrected


def interactive_correction_review(corrections, word_counts, max_review=50):
    """Interactively review and approve corrections."""
    
    approved_corrections = {}
    
    print(f"\nReviewing up to {max_review} corrections...")
    print("Enter 'y' to approve, 'n' to reject, 'q' to quit review")
    
    for i, (wrong, correct) in enumerate(list(corrections.items())[:max_review]):
        wrong_freq = word_counts[wrong]
        correct_freq = word_counts[correct]
        
        response = input(f"\n{wrong} (freq:{wrong_freq}) -> {correct} (freq:{correct_freq})? [y/n/q]: ")
        
        if response.lower() == 'q':
            break
        elif response.lower() == 'y':
            approved_corrections[wrong] = correct
    
    print(f"\nApproved {len(approved_corrections)} corrections")
    return approved_corrections


# Example usage function to integrate with your pipeline
def enhanced_spell_check(df):
    """Enhanced spell checking using frequency analysis."""
    
    # Get frequency-based corrections
    corrections = frequency_based_correction(df, min_frequency=3, max_distance=2)
    
    # Optional: Interactive review
    # word_counts = Counter()
    # for text in df['OCCUPATION_DESC1_temp'].dropna():
    #     word_counts.update(text.lower().split())
    # corrections = interactive_correction_review(corrections, word_counts)
    
    # Apply corrections
    df_corrected = apply_corrections(df, corrections)
    
    return df_corrected
