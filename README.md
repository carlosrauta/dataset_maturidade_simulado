# Dataset de Maturidade de Espécie Pesqueira (Estudo de **$L_{50}$**)

Este repositório contém um conjunto de dados fictício com 800 registos de uma espécie de peixe hipotética. O dataset foi estruturado especificamente para fins académicos e de treino em avaliação de recursos pesqueiros, focando-se na estimativa do **$L_{50}$** (comprimento médio de primeira maturação).

## Contexto Biológico

O **$L_{50}$** é o comprimento no qual 50% dos indivíduos de uma população são considerados sexualmente maduros. Este parâmetro é crucial para a gestão pesqueira, pois permite definir o **Tamanho Mínimo de Captura (TMC)**, garantindo que pelo menos metade da população teve a oportunidade de se reproduzir pelo menos uma vez antes de ser capturada.

## Estrutura do Dataset

O ficheiro `dataset_maturidade_peixes_800.csv` contém as seguintes colunas:

| Coluna | Descrição | Tipo |
| :--- | :--- | :--- |
| `ID` | Identificador único do espécime | Inteiro |
| `Comprimento_Total_cm` | Comprimento total do indivíduo (cm) | Decimal |
| `Peso_g` | Peso total do indivíduo (g) | Decimal |
| `Sexo` | Sexo do indivíduo (F - Fêmea, M - Macho) | Categórico |
| `Estagio_Gonadal` | Estágio de maturação macroscópica (I, II, III, IV) | Ordinal |
| `Maturidade_Binaria` | Classificação binária (0 = Imaturo, 1 = Maduro) | Booleano |

### Critérios de Classificação
Para facilitar a modelação logística, os estágios gonadais foram agrupados:
- **Imaturos (0):** Estágios I e II (indivíduos jovens ou em repouso).
- **Maduros (1):** Estágios III e IV (indivíduos em maturação avançada ou desovados).

## Sugestões de Análise

- Estimativa de **$L_{50}$** geral
- Comparação entre machos e fêmeas
- Construção de curva logística
- Relação peso-comprimento

## Estimativa da L50

A L50 pode ser estimada com um modelo logístico:

**$\[
P(maduro) = \frac{1}{1 + e^{-(a + b \cdot comprimento)}}
$\]**

- `P(maduro)` = probabilidade de estar maduro  
- `a`, `b` = parâmetros estimados  
- **$L_{50} = \(-a/b\)$**

## Ferramentas Recomendadas

- R: `glm`, `tidyverse`, `ggplot2`
- Python: `statsmodels`, `scikit-learn`, `matplotlib`
- Outros softwares: SPSS, SAS, Excel para análises exploratórias
