#!/usr/bin/env python
# coding: utf-8

# ### Read in data

# In[18]:


import pandas as pd
df = pd.read_csv("../../data/data.csv")


# ### Isolate occupation and occupation code

# In[19]:


occupation = df[["OCCUPATION_CODE", "OCCUPATION_DESC1"]]


# In[20]:


occupation.isna().sum()


# In[21]:


occupation["OCCUPATION_DESC1"].head(50)


# In[22]:


occupation = occupation.copy()
occupation["OCCUPATION_DESC1"] = (
    occupation["OCCUPATION_DESC1"]
    .str.lower()
    .str.replace(r'[^\w\s]', ' ', regex=True)  # keep only letters, numbers, and spaces
)


# In[23]:


import pandas as pd

# Step 1: Compute the most frequent code per description
most_freq_codes = (
    occupation[occupation["OCCUPATION_CODE"].notna()]  # filter out missing codes
    .groupby(["OCCUPATION_DESC1", "OCCUPATION_CODE"])
    .size()  # count occurrences
    .reset_index(name="freq")
    .sort_values(["OCCUPATION_DESC1", "freq"], ascending=[True, False])
    .drop_duplicates(subset=["OCCUPATION_DESC1"], keep="first")[["OCCUPATION_DESC1", "OCCUPATION_CODE"]]
)

# Step 2: Fill missing codes with the most frequent code
df_filled = occupation.merge(
    most_freq_codes, 
    on="OCCUPATION_DESC1", 
    how="left", 
    suffixes=("", "_mostfreq")
)

df_filled["OCCUPATION_CODE"] = df_filled["OCCUPATION_CODE"].fillna(df_filled["OCCUPATION_CODE_mostfreq"])

# Keep only the columns we care about
occupation = df_filled[["OCCUPATION_CODE", "OCCUPATION_DESC1"]]


# In[24]:


occupation.isna().sum()


# In[25]:


import pandas as pd

# Step 1: Compute most frequent description per code
most_freq_desc = (
    occupation[occupation["OCCUPATION_DESC1"].notna()]  # filter out missing descriptions
    .groupby(["OCCUPATION_CODE", "OCCUPATION_DESC1"])
    .size()  # count occurrences
    .reset_index(name="freq")
    .sort_values(["OCCUPATION_CODE", "freq"], ascending=[True, False])
    .drop_duplicates(subset=["OCCUPATION_CODE"], keep="first")[["OCCUPATION_CODE", "OCCUPATION_DESC1"]]
)

# Step 2: Fill missing descriptions
df_filled_desc = occupation.merge(
    most_freq_desc,
    on="OCCUPATION_CODE",
    how="left",
    suffixes=("", "_mostfreq")
)

df_filled_desc["OCCUPATION_DESC1"] = df_filled_desc["OCCUPATION_DESC1"].fillna(df_filled_desc["OCCUPATION_DESC1_mostfreq"])

# Keep only the desired columns
occupation = df_filled_desc[["OCCUPATION_CODE", "OCCUPATION_DESC1"]]


# In[26]:


occupation.isna().sum()


# In[27]:


len(occupation["OCCUPATION_DESC1"].unique())


# In[28]:


occupation["OCCUPATION_DESC1"].head(50)


# In[29]:


from spellchecker import SpellChecker
import pandas as pd
from tqdm import tqdm  # progress bar

spell = SpellChecker()

def fix_text(text):
    if pd.isna(text):
        return text
    # Remove anything after '/' or ':'
    # text = text.split('/')[0].split(':')[0].strip()
    words = text.split()
    corrected_words = []
    for w in words:
        corrected = spell.correction(w)
        if corrected is None:
            print(f"Unrecognized word: {w}")  # print original word if no correction
            corrected = w
        corrected_words.append(corrected)
    return ' '.join(corrected_words).lower()  # also lowercase

# Extract unique occupations
unique_occupations = occupation['OCCUPATION_DESC1'].dropna().unique()
print(f"Number of unique occupations: {len(unique_occupations)}")

# Clean with progress bar
unique_occupations_cleaned = [fix_text(x) for x in tqdm(unique_occupations, desc="Cleaning unique occupations")]

# Create mapping to apply to full column
cleaning_map = dict(zip(unique_occupations, unique_occupations_cleaned))
occupation['OCCUPATION_DESC1_clean'] = occupation['OCCUPATION_DESC1'].map(cleaning_map)

# Preview results
print(occupation[['OCCUPATION_DESC1', 'OCCUPATION_DESC1_clean']].head(10))


# In[30]:


# Manual mapping for unrecognized / custom occupations
manual_map = {
    "bottlestore": "alchol retailer",
    "panelbeater": "panel beater",
    "debswana": "mining", # de beers botswana
    "nightwatch": "night watchman",
    "groundslady": "grounds lady",
    "ramotswa": "unknown", # a village in bots
    "procurementofficer": "procurement officer",
    "businessdevelopment": "business development",
    "draughtsman": "draughtsman",
    "hydrogeologist": "hydro geologist",
    "crewboss": "crew boss",
    "wellfair": "welfare",
    "assistastant": "assistant",
    "morupule": "mining", # coal mine in bots
    "groundlady": "ground lady",
    "machineman": "machine man",
    "heradboy": "herald boy",
    "letshego": "finance", # letshego is a bots finance company
    "accountscontroller": "accounts controller",
    "pharmacotherapist": "pharma cotherapist",
    "pipefitter": "pipe fitter",
    "supretendan": "superintendent",
    "machendezer": "merchandiser",
    "laundrylady": "laundry lady",
    ### things i am adding cause i feel like it 
    "skilled semi skilled": "skilled worker",
    "waist packer": "waste packer"
}

# Apply this mapping on top of the previous cleaned column
occupation['OCCUPATION_DESC1_clean'] = occupation['OCCUPATION_DESC1_clean'].replace(manual_map)

# Preview results
print(occupation[['OCCUPATION_DESC1', 'OCCUPATION_DESC1_clean']].head(1000))


# In[31]:


len(occupation["OCCUPATION_DESC1"].unique())


# In[32]:


# List of generic "role" words
generic_words = {
    "worker","assistant","officer","staff","personnel","manager","director",
    "supervisor","administrator","registrar","practitioner","controller",
    "operator","keeper","hand","attendant","clerk","cashier","secretary",
    "receptionist","messenger","guard","soldier","watchman","orderly",
    "sister","teacher","educator","minor","unknown"
}

def preprocess_occupation(title: str) -> str:
    if pd.isna(title): 
        return title
    
    words = title.lower().split()
    
    # if it's a single word, keep it as is
    if len(words) == 1:
        return title.lower().strip()
    
    # otherwise remove generic words
    cleaned = [w for w in words if w not in generic_words]
    
    # if everything got removed, fall back to original
    if not cleaned:
        return title.lower().strip()
    
    return " ".join(cleaned).strip()

# Apply to dataset
occupation["OCCUPATION_DESC1_clean"] = occupation["OCCUPATION_DESC1_clean"].apply(preprocess_occupation)


# In[33]:


len(occupation["OCCUPATION_DESC1_clean"].unique())


# In[35]:


import umap.umap_ as umap
from sentence_transformers import SentenceTransformer

model = SentenceTransformer('multi-qa-MiniLM-L6-cos-v1')
unique_occupations_clean = occupation['OCCUPATION_DESC1_clean'].dropna().unique()
embeddings = model.encode(unique_occupations_clean)

reducer = umap.UMAP(
    n_neighbors=15,      # preserves local structure
    n_components=5,      # for clustering; use 2 if for visualization only
    metric='cosine',     # correct for embeddings
    random_state=42,
)
embeddings_reduced = reducer.fit_transform(embeddings)


# In[36]:


import matplotlib.pyplot as plt
import itertools
import numpy as np

# embeddings_reduced.shape = (n_samples, 5)
n_components = embeddings_reduced.shape[1]

fig, axes = plt.subplots(n_components, n_components, figsize=(15,15))
plt.subplots_adjust(wspace=0.3, hspace=0.3)

for i in range(n_components):
    for j in range(n_components):
        ax = axes[i, j]
        if i == j:
            # Diagonal: maybe show histogram
            ax.hist(embeddings_reduced[:, i], bins=30, color='skyblue')
            ax.set_ylabel(f"Dim {i}")
            ax.set_xlabel(f"Dim {j}")
        else:
            # Scatter off-diagonal
            ax.scatter(embeddings_reduced[:, j], embeddings_reduced[:, i], s=5, alpha=0.6)
            ax.set_xlabel(f"Dim {j}")
            ax.set_ylabel(f"Dim {i}")

plt.suptitle("Pairwise scatterplots of 5D UMAP embeddings", fontsize=16)
plt.show()


# In[37]:


import matplotlib.pyplot as plt
from sklearn.cluster import KMeans
from sklearn.metrics import silhouette_score

# Try different values of k
inertia = []
sil_scores = []
K = range(2, 100)  # test from 2 to 40 clusters

for k in K:
    kmeans = KMeans(n_clusters=k, random_state=42, n_init=10)
    kmeans.fit(embeddings_reduced)
    inertia.append(kmeans.inertia_)  # sum of squared distances
    sil_scores.append(silhouette_score(embeddings_reduced, kmeans.labels_))

# Plot elbow curve (inertia)
plt.figure(figsize=(10,5))
plt.plot(K, inertia, 'bx-')
plt.xlabel('k (Number of clusters)')
plt.ylabel('Inertia (Within-cluster SSE)')
plt.title('Elbow Method For Optimal k')
plt.show()

# Plot silhouette scores
plt.figure(figsize=(10,5))
plt.plot(K, sil_scores, 'rx-')
plt.xlabel('k (Number of clusters)')
plt.ylabel('Silhouette Score')
plt.title('Silhouette Scores for different k')
plt.show()


# In[42]:


# -----------------------------
# 6. Cluster with KMeans
# -----------------------------
num_clusters = 20  # choose how many clusters you want
kmeans = KMeans(n_clusters=num_clusters, random_state=42)
clusters = kmeans.fit_predict(embeddings_reduced)

# -----------------------------
# 7. Map clusters back to unique occupations
# -----------------------------
occupation_cluster_map = pd.DataFrame({
    "OCCUPATION_DESC1_clean": unique_occupations_clean,
    "cluster": clusters
})

# Merge back to full dataset
if "cluster" in occupation.columns:
    occupation = occupation.drop(columns=["cluster"])

occupation = occupation.merge(
    occupation_cluster_map, 
    on="OCCUPATION_DESC1_clean", 
    how="left"
)


# -----------------------------
# 8. Inspect clusters
# -----------------------------
# Number of clusters
print(f"Number of clusters: {len(set(clusters))}")

# Words per cluster
clustered_words = occupation_cluster_map.groupby("cluster")["OCCUPATION_DESC1_clean"].apply(list)
cluster_sizes = occupation_cluster_map.groupby("cluster").size()
print("\nCluster sizes:")
print(cluster_sizes)

# Show top 10 occupations per cluster
for cluster_id, words in clustered_words.items():
    print(f"\nCluster {cluster_id} ({len(words)} occupations):")
    print(words[:20])


# In[43]:


import matplotlib.pyplot as plt
import numpy as np

# Suppose:
# embeddings_reduced.shape = (n_samples, 5)
# clusters = array of KMeans/HDBSCAN cluster labels for each point

n_components = embeddings_reduced.shape[1]

fig, axes = plt.subplots(n_components, n_components, figsize=(15,15))
plt.subplots_adjust(wspace=0.3, hspace=0.3)

for i in range(n_components):
    for j in range(n_components):
        ax = axes[i, j]
        if i == j:
            # Diagonal: histogram colored by cluster
            for c in np.unique(clusters):
                ax.hist(
                    embeddings_reduced[clusters==c, i],
                    bins=20, alpha=0.6, label=f"Cluster {c}"
                )
            if i == 0:
                ax.legend(fontsize=8)
            ax.set_xlabel(f"Dim {j}")
            ax.set_ylabel(f"Dim {i}")
        else:
            # Off-diagonal: scatter plot colored by cluster
            for c in np.unique(clusters):
                ax.scatter(
                    embeddings_reduced[clusters==c, j],
                    embeddings_reduced[clusters==c, i],
                    s=5, alpha=0.6, label=f"Cluster {c}" if i==0 and j==1 else ""
                )
            ax.set_xlabel(f"Dim {j}")
            ax.set_ylabel(f"Dim {i}")

plt.suptitle("Pairwise 5D UMAP embeddings with cluster coloring", fontsize=16)
plt.show()


# In[44]:


import umap.umap_ as umap

# 2D projection for visualization
reducer_2d = umap.UMAP(
    n_neighbors=15,      # preserves local structure
    n_components=2,      # for clustering; use 2 if for visualization only
    metric='cosine',     # correct for embeddings
    random_state=42,
)
embeddings_2d = reducer_2d.fit_transform(embeddings)  # embeddings = original SentenceTransformer embeddings

import matplotlib.pyplot as plt

plt.figure(figsize=(10,7))
scatter = plt.scatter(
    embeddings_2d[:,0], embeddings_2d[:,1], 
    c=clusters, cmap='tab20', s=10
)
plt.title("KMeans Clusters of Occupations (2D UMAP)")
plt.xlabel("UMAP 1")
plt.ylabel("UMAP 2")
plt.colorbar(scatter, label="Cluster")
plt.show()



# In[51]:


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
    ['chef', 'cook', 'waiter'] -> Hospitality / Food Services
    ['farmer', 'veterinary', 'wildlife'] -> Agriculture / Environmental
    ['computer', 'IT', 'librarian'] -> Information Technology / Knowledge Work

    Occupations: [{occupations_str}]
    Sector:"""

    
    result = classifier(prompt, max_new_tokens=10)
    return result[0]["generated_text"].strip()

# Get cluster mappings
cluster_occupations = {}
for cluster_id in occupation['cluster'].dropna().unique():
    cluster_occs = occupation[occupation['cluster'] == cluster_id]['OCCUPATION_DESC1_clean'].dropna().unique()
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
occupation['ai_sector'] = occupation['cluster'].map(cluster_sectors)

print(f"\nAI Sector distribution:")
print(occupation['ai_sector'].value_counts())


# In[52]:


import matplotlib.pyplot as plt

# Count occupations per sector
sector_counts = occupation["ai_sector"].value_counts()

# Plot histogram
plt.figure(figsize=(12,6))
sector_counts.plot(kind='bar', color='skyblue')
plt.xlabel("Sector")
plt.ylabel("Number of Occupations")
plt.title("Number of Occupations per Sector")
plt.xticks(rotation=45, ha='right')
plt.show()


# In[193]:


# import pandas as pd
# from transformers import AutoTokenizer, AutoModelForSeq2SeqLM, pipeline
# from tqdm import tqdm  # progress bar

# # Example dataframe
# # occupation = pd.DataFrame({'OCCUPATION_DESC1': [...your data...]})

# # Load a small instruction-tuned model
# model_name = "google/flan-t5-large"
# tokenizer = AutoTokenizer.from_pretrained(model_name)
# model = AutoModelForSeq2SeqLM.from_pretrained(model_name)

# # Create a text2text pipeline
# cleaner = pipeline(
#     "text2text-generation", model=model, tokenizer=tokenizer, device=-1, max_new_tokens=32
# )  # use GPU if available


# # Function to clean and correct a single occupation
# def clean_occupation(text):
#     if pd.isna(text):
#         return text
#     prompt = (
#         "Convert occupation descriptions to a single short standard term without losing the meaning of the occupation, lowercase:\n"
#         "teacher assistant -> teacher assistant\n"
#         "salesman saleslady other -> salesman\n"
#         "supplies officer -> supplies officer\n"
#         "credit controller -> credit controller\n"
#         "quality controller -> quality controller\n"
#         "skilled semi skilled -> skilled worker\n"
#         "nursing matrons or sisters and qualified nurse -> nurse\n"
#         "assistant general manager -> assistant general manager\n"
#         "head teacher -> teacher\n"
#         "nurse orderly assistant -> nurse orderly assistant\n"
#         "salesman saleslady travelling -> salesman saleslady\n"
#         "computer system analyst scientist engineer -> scientist\n"
#         "night watchman -> watchman\n"
#         "fire officer -> fire officer\n"
#         "store keeper shop owner -> shop owner\n"
#         "technical officer building -> technical officer\n"
#         "plant operator assistant -> plant operator\n"
#         "personal assistant -> personal assistant\n"
#         "accounts clerk -> accounts clerk\n"
#         "school principal -> school principal\n"

#         f"{text} ->"
#     )

#     output = cleaner(prompt, max_new_tokens=32, do_sample=False)
#     cleaned_text = output[0]["generated_text"].strip()
#     print(f"Input: {text}  -->  Output: {cleaned_text}")  # print each mapping
#     return cleaned_text


# # Extract unique occupation descriptions
# unique_occupations = occupation["OCCUPATION_DESC1_clean"].dropna().unique()
# print(f"Number of unique occupations: {len(unique_occupations)}")

# # Clean unique occupations with a progress bar
# unique_cleaned = []
# for occ in tqdm(unique_occupations, desc="Cleaning unique occupations"):
#     unique_cleaned.append(clean_occupation(occ))

# # Create mapping from original -> cleaned
# cleaning_map = dict(zip(unique_occupations, unique_cleaned))

# # Apply mapping to the full dataframe
# occupation["OCCUPATION_DESC1_clean_llm"] = occupation["OCCUPATION_DESC1_clean"].map(
#     cleaning_map
# )

# # Preview results
# print(occupation[["OCCUPATION_DESC1_clean", "OCCUPATION_DESC1_clean_llm"]].head(10))

