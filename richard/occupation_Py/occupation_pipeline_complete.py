import pandas as pd
import numpy as np
from spellchecker import SpellChecker
from sentence_transformers import SentenceTransformer
import umap.umap_ as umap
from sklearn.cluster import KMeans
from tqdm import tqdm


def read_data(filepath="../../data/data.csv"):
    """Read the full dataset."""
    return pd.read_csv(filepath)


def text_cleaning(df):
    """Clean occupation text - lowercase and remove punctuation."""
    df = df.copy()
    df["OCCUPATION_DESC1_temp"] = (
        df["OCCUPATION_DESC1"]
        .str.lower()
        .str.replace(r'[^\w\s]', ' ', regex=True)
    )
    return df


def fill_missing_codes(df):
    """Fill missing occupation codes using most frequent code per description."""
    most_freq_codes = (
        df[df["OCCUPATION_CODE"].notna()]
        .groupby(["OCCUPATION_DESC1_temp", "OCCUPATION_CODE"])
        .size()
        .reset_index(name="freq")
        .sort_values(["OCCUPATION_DESC1_temp", "freq"], ascending=[True, False])
        .drop_duplicates(subset=["OCCUPATION_DESC1_temp"], keep="first")[["OCCUPATION_DESC1_temp", "OCCUPATION_CODE"]]
    )
    
    df_filled = df.merge(
        most_freq_codes, 
        on="OCCUPATION_DESC1_temp", 
        how="left", 
        suffixes=("", "_mostfreq")
    )
    
    df_filled["OCCUPATION_CODE"] = df_filled["OCCUPATION_CODE"].fillna(df_filled["OCCUPATION_CODE_mostfreq"])
    df_filled = df_filled.drop(columns=["OCCUPATION_CODE_mostfreq"], errors='ignore')
    return df_filled


def fill_missing_descriptions(df):
    """Fill missing occupation descriptions using most frequent description per code."""
    most_freq_desc = (
        df[df["OCCUPATION_DESC1_temp"].notna()]
        .groupby(["OCCUPATION_CODE", "OCCUPATION_DESC1_temp"])
        .size()
        .reset_index(name="freq")
        .sort_values(["OCCUPATION_CODE", "freq"], ascending=[True, False])
        .drop_duplicates(subset=["OCCUPATION_CODE"], keep="first")[["OCCUPATION_CODE", "OCCUPATION_DESC1_temp"]]
    )
    
    df_filled = df.merge(
        most_freq_desc,
        on="OCCUPATION_CODE",
        how="left",
        suffixes=("", "_mostfreq")
    )
    
    df_filled["OCCUPATION_DESC1_temp"] = df_filled["OCCUPATION_DESC1_temp"].fillna(df_filled["OCCUPATION_DESC1_temp_mostfreq"])
    df_filled = df_filled.drop(columns=["OCCUPATION_DESC1_temp_mostfreq"], errors='ignore')
    return df_filled


def spell_check(df):
    """Apply spell checking to occupation descriptions."""
    spell = SpellChecker()
    
    def fix_text(text):
        if pd.isna(text):
            return text
        words = text.split()
        corrected_words = []
        for w in words:
            corrected = spell.correction(w)
            if corrected is None:
                corrected = w
            corrected_words.append(corrected)
        return ' '.join(corrected_words).lower()
    
    unique_occupations = df['OCCUPATION_DESC1_temp'].dropna().unique()
    print(f"Spell checking {len(unique_occupations)} unique occupations...")
    
    unique_occupations_cleaned = [fix_text(x) for x in tqdm(unique_occupations, desc="Spell checking")]
    
    cleaning_map = dict(zip(unique_occupations, unique_occupations_cleaned))
    df['OCCUPATION_DESC1_clean'] = df['OCCUPATION_DESC1_temp'].map(cleaning_map)
    
    return df


def manual_correction(df):
    """Apply manual corrections for domain-specific terms."""
    manual_map = {
        "bottlestore": "alcohol retailer",
        "panelbeater": "panel beater",
        "debswana": "mining",
        "nightwatch": "night watchman",
        "groundslady": "grounds lady",
        "ramotswa": "unknown",
        "procurementofficer": "procurement officer",
        "businessdevelopment": "business development",
        "draughtsman": "draughtsman",
        "hydrogeologist": "hydro geologist",
        "crewboss": "crew boss",
        "wellfair": "welfare",
        "assistastant": "assistant",
        "morupule": "mining",
        "groundlady": "ground lady",
        "machineman": "machine man",
        "heradboy": "herald boy",
        "letshego": "finance",
        "accountscontroller": "accounts controller",
        "pharmacotherapist": "pharma cotherapist",
        "pipefitter": "pipe fitter",
        "supretendan": "superintendent",
        "machendezer": "merchandiser",
        "laundrylady": "laundry lady",
        "skilled semi skilled": "skilled worker",
        "waist packer": "waste packer"
    }
    
    df['OCCUPATION_DESC1_clean'] = df['OCCUPATION_DESC1_clean'].replace(manual_map)
    return df


def remove_generic_words(df):
    """Remove generic role words from occupation descriptions."""
    generic_words = {
        "worker","assistant","officer","staff","personnel","manager","director",
        "supervisor","administrator","registrar","practitioner","controller",
        "operator","keeper","hand","attendant","clerk","cashier","secretary",
        "receptionist","messenger","guard","soldier","watchman","orderly",
        "sister","teacher","educator","minor","unknown"
    }
    
    def preprocess_occupation(title):
        if pd.isna(title): 
            return title
        
        words = title.lower().split()
        
        if len(words) == 1:
            return title.lower().strip()
        
        cleaned = [w for w in words if w not in generic_words]
        
        if not cleaned:
            return title.lower().strip()
        
        return " ".join(cleaned).strip()
    
    df["OCCUPATION_DESC1_clean"] = df["OCCUPATION_DESC1_clean"].apply(preprocess_occupation)
    return df


def embed(df):
    """Create embeddings for unique occupations."""
    print("Creating embeddings...")
    model = SentenceTransformer('multi-qa-MiniLM-L6-cos-v1')
    unique_occupations_clean = df['OCCUPATION_DESC1_clean'].dropna().unique()
    embeddings = model.encode(unique_occupations_clean)
    
    return df, embeddings, unique_occupations_clean


def reduce_dimensions(df, embeddings, unique_occupations_clean):
    """Reduce dimensionality using UMAP."""
    print("Reducing dimensions...")
    reducer = umap.UMAP(
        n_neighbors=15,
        n_components=5,
        metric='cosine',
        random_state=42,
    )
    embeddings_reduced = reducer.fit_transform(embeddings)
    
    return df, embeddings_reduced, unique_occupations_clean


def cluster_occupations(df, embeddings_reduced, unique_occupations_clean, n_clusters=20):
    """Cluster occupations using KMeans."""
    print(f"Clustering into {n_clusters} clusters...")
    kmeans = KMeans(n_clusters=n_clusters, random_state=42)
    clusters = kmeans.fit_predict(embeddings_reduced)
    
    occupation_cluster_map = pd.DataFrame({
        "OCCUPATION_DESC1_clean": unique_occupations_clean,
        "cluster": clusters
    })
    
    df = df.merge(occupation_cluster_map, on="OCCUPATION_DESC1_clean", how="left")
    
    return df


def assign_sectors_with_t5(df):
    """Use T5 to classify clusters into sectors like the notebook."""
    from transformers import pipeline
    
    # Load T5 classifier
    classifier = pipeline("text2text-generation", model="google/flan-t5-large")
    
    def classify_occupations(occupation_list):
        occupations_str = ", ".join(occupation_list[:10])  # Limit to first 10 to avoid token limits
        prompt = f"""Classify these occupations into a single broad sector category using these examples:

['social', 'minor', 'nurse'] -> Healthcare / Social Services
['driver other than chauffeur', 'fireman', 'fire'] -> Transportation / Emergency Services
['security', 'police', 'traffic'] -> Security / Policing / Army
['mechanic', 'engineer', 'technician'] -> Technical / Engineering
['teacher', 'education', 'school'] -> Education
['finance', 'accounting', 'bank'] -> Finance / Banking
['retail', 'sales', 'shop'] -> Retail / Sales

Occupations: [{occupations_str}]
Sector:"""
        
        result = classifier(prompt, max_new_tokens=15)
        return result[0]["generated_text"].strip()
    
    # Get cluster mappings
    cluster_occupations = {}
    for cluster_id in df['cluster'].dropna().unique():
        cluster_occs = df[df['cluster'] == cluster_id]['OCCUPATION_DESC1_clean'].dropna().unique()
        cluster_occupations[cluster_id] = list(cluster_occs)
    
    # Classify each cluster
    cluster_sectors = {}
    print("Classifying clusters with T5...")
    
    for cluster_id, occupations in tqdm(cluster_occupations.items(), desc="Classifying clusters"):
        if len(occupations) > 0:
            sector = classify_occupations(occupations)
            cluster_sectors[cluster_id] = sector
            print(f"Cluster {cluster_id}: {sector} (sample: {occupations[:3]})")
    
    # Map back to dataframe
    df['sector'] = df['cluster'].map(cluster_sectors)
    
    return df


def save_data(df, output_path="processed_data_with_sectors.csv"):
    """Save the processed dataset."""
    df.to_csv(output_path, index=False)
    print(f"Data saved to: {output_path}")
    return df


def cleanup_temp_columns(df):
    """Remove temporary processing columns but keep OCCUPATION_DESC1_clean."""
    columns_to_drop = ["OCCUPATION_DESC1_temp", "cluster"]
    df = df.drop(columns=columns_to_drop, errors='ignore')
    return df


def occupation_processing_pipeline(filepath="../../data/data.csv"):
    """Complete occupation processing pipeline."""
    print("Starting occupation processing pipeline...")
    
    # Chain all processing steps
    df = read_data(filepath)
    df = text_cleaning(df)
    df = fill_missing_codes(df)
    df = fill_missing_descriptions(df)
    df = spell_check(df)
    df = manual_correction(df)
    df = remove_generic_words(df)
    
    # Embedding and clustering steps
    df, embeddings, unique_occupations = embed(df)
    df, embeddings_reduced, unique_occupations = reduce_dimensions(df, embeddings, unique_occupations)
    df = cluster_occupations(df, embeddings_reduced, unique_occupations)
    df = assign_sectors_with_t5(df)  # Use T5 instead of manual mapping
    df = cleanup_temp_columns(df)
    df = save_data(df)  # Save the processed data
    
    print("Pipeline complete!")
    print(f"Dataset shape: {df.shape}")
    print(f"Sector distribution:")
    print(df['sector'].value_counts())
    
    return df


if __name__ == "__main__":
    result_df = occupation_processing_pipeline()
    print(f"\nFinal dataset has {len(result_df)} records with new 'sector' column")
