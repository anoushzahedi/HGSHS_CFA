# Can hypnotic susceptibility be explained by bifactor models?
### Structural equation modeling of the Harvard group scale of hypnotic susceptibility - Form A

**Author:** Anoushiravan Zahedi  
**Publication:** *Consciousness and Cognition* (2022)  
**DOI:** [10.1016/j.concog.2022.103289](https://doi.org/10.1016/j.concog.2022.103289)

---

## Abstract
Individuals differ significantly in their responsiveness to hypnotic suggestions. However, defining and measuring hypnotizability remains contentious because standardized scales, such as the **Harvard Group Scale of Hypnotic Susceptibility (HGSHS:A)**, measure a mixture of general suggestibility and its alteration due to hypnotic induction.

While exploratory factor analyses (EFA) have indicated multidimensionality, the exact nature of these latent factors is debated. In this study, we applied **Confirmatory Factor Analysis (CFA)** to the HGSHS:A scores of 477 volunteers to test several theory-driven models. 

### Key Findings:
- **Bifactor Model Fit**: Scores were best explained by a bifactor model consisting of a **General (G) factor** and three correlated minor factors.
- **Dual Sources of Variability**: The model demonstrates that two sources of variability affect HGSHS:A scores simultaneously.
- **Predictive Structure**: Structural Equation Modeling (SEM) revealed that the **challenge-ideomotor factor** predicts the other two minor factors, suggesting these suggestions may involve more fundamental psychological processes.
- **Implications**: These results underscore the multifaceted structure of hypnotic suggestibility and highlight the need for more differentiated measurement scales.

---

## Repository Content
This repository contains the data and analysis scripts required to reproduce the findings of the study:

- **Data**: Raw and processed HGSHS:A scores (N=477).
- **Analysis Scripts**: 
  - Confirmatory Factor Analysis (CFA) models.
  - Structural Equation Modeling (SEM) scripts.
  - Model comparison and fit statistics.

## Usage
The analyses were primarily conducted using [R/Mplus/Python - *Please specify if different*]. To replicate the models:
1. Ensure you have the required SEM libraries installed (e.g., `lavaan` in R).
2. Load the dataset provided in the `/data` folder.
3. Run the scripts in the `/scripts` folder sequentially.

## Citation
If you use this data or code in your research, please cite the original paper:

```bibtex
@article{zahedi2022hypnotic,
  title={Can hypnotic susceptibility be explained by bifactor models? Structural equation modeling of the Harvard group scale of hypnotic susceptibility - Form A},
  author={Zahedi, Anoushiravan and Sommer, Werner},
  journal={Consciousness and Cognition},
  volume={99},
  pages={103289},
  year={2022},
  publisher={Elsevier},
  doi={10.1016/j.concog.2022.103289}
}
```

## License
This repository is provided for academic and research purposes. Please refer to the LICENSE file for more details.
