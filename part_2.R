# naive bayes or hhm
# this is not by any means finidhed, feel free to totally overwrite this. was just putting a general idea

library(e1071)

classify_sequence <- function(seq1, seq2, match_score, mismatch_score, gap_penalty) {
  alignment_result <- global_alignment(seq1, seq2, match_score, mismatch_score, gap_penalty)
  
  matched_residues <- sum(strsplit(alignment_result$align_seq1, "")[[1]] == strsplit(alignment_result$align_seq2, "")[[1]])
  mismatched_residues <- alignment_result$alignment_score - matched_residues
  gaps <- sum(strsplit(alignment_result$align_seq1, "")[[1]] == "-" | strsplit(alignment_result$align_seq2, "")[[1]] == "-")
  
  data <- data.frame(
    Matched = matched_residues,
    Mismatched = mismatched_residues,
    Gaps = gaps,
    Class = c("bird", "mammalian")
  )
  
  nb_classifier <- naiveBayes(Class ~ ., data = data)
  
  return(nb_classifier)
}

# new_data <- data.frame(MatchedResidues = x, MismatchedResidues = x, Gaps = x)
# predicted_class <- predict(classifier, new_data)
