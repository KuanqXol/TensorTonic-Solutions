import numpy as np
from typing import List, Dict

class SimpleTokenizer:
    """
    A word-level tokenizer with special tokens.
    """
    
    def __init__(self):
        self.word_to_id: Dict[str, int] = {}
        self.id_to_word: Dict[int, str] = {}
        self.vocab_size = 0
        
        # Special tokens
        self.pad_token = "<PAD>"
        self.unk_token = "<UNK>"
        self.bos_token = "<BOS>"
        self.eos_token = "<EOS>"
    
    def build_vocab(self, texts: List[str]) -> None:
        """
        Build vocabulary from a list of texts.
        Add special tokens first, then unique words.
        """
        # YOUR CODE HERE
        vocab = set()
        
        for text in texts:
            words = text.lower().split()
            vocab.update(words)
        
        # Sort alphabetically
        vocab = sorted(vocab)
        
        # Add special tokens
        special_tokens = [
            self.pad_token,
            self.unk_token,
            self.bos_token,
            self.eos_token
        ]
        
        # Build word_to_id
        for idx, token in enumerate(special_tokens):
            self.word_to_id[token] = idx
        
        # Add normal words
        current_idx = len(special_tokens)
        
        for word in vocab:
            self.word_to_id[word] = current_idx
            current_idx += 1
        
        # Build id_to_word
        self.id_to_word = {
            idx: word
            for word, idx in self.word_to_id.items()
        }
        
        self.vocab_size = len(self.word_to_id)
        pass
    
    def encode(self, text: str) -> List[int]:
        """
        Convert text to list of token IDs.
        Use UNK for unknown words.
        """
        # YOUR CODE HERE
        words = text.lower().split()
        
        ids = []
        
        for word in words:
            if word in self.word_to_id:
                ids.append(self.word_to_id[word])
            else:
                ids.append(self.word_to_id[self.unk_token])
        
        return ids
        pass
    
    def decode(self, ids: List[int]) -> str:
        """
        Convert list of token IDs back to text.
        """
        # YOUR CODE HERE
        words = []

        for idx in ids:
            if idx in self.id_to_word:
                words.append(self.id_to_word[idx])
            else:
                words.append(self.unk_token)

        return " ".join(words)
        pass
