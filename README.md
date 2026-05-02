<p align="center">
  <img src="https://img.shields.io/badge/-4.3.2%20%7C%204.5.1-276DC3?style=for-the-badge&logo=r&logoColor=white"/>
  <img src="https://img.shields.io/badge/environment-renv-009E73?style=for-the-badge"/>
  <img src="https://img.shields.io/github/actions/workflow/status/drewmelo/EWA-MarketDynamics/check-r.yml?style=for-the-badge&label=CI"/>
  <img src="https://img.shields.io/github/v/release/drewmelo/EWA-MarketDynamics?style=for-the-badge&label=version"/>
  <img src="https://img.shields.io/badge/models-EWA%20%7C%20RL%20%7C%20BL-6f42c1?style=for-the-badge"/>
  <img src="https://img.shields.io/badge/app-Shiny-75AADB?style=for-the-badge"/>
</p>

<div align="center">

  <p align="center">
    <img src="https://raw.githubusercontent.com/drewmelo/EWA-MarketDynamics/refs/heads/master/assets/logo-main.png" alt="Logo" width="110">
  </p>

  <h1 align="center" style="margin-bottom: 0;">
    Dinâmicas de Aprendizado em Cenários de Incertezas de Mercado
  </h1>

  <h3 align="center" style="margin-top: 5px; margin-bottom: 20px;">
    Aplicação de teoria dos jogos com Experience-Weighted Attraction
  </h3>

  <p align="center" style="margin-top: 25px;">
    Trabalho de Conclusão de Curso<br/>
    <sub>Defesa: data</sub>
  </p>

  <p align="center" style="margin-top: 10px;">
    📖 <a href="https://drewmelo.github.io/EWA-MarketDynamics/">Ler TCC (versão digital)</a> ·
    🐛 <a href="https://github.com/drewmelo/EWA-MarketDynamics/issues/new?labels=bug&template=bug_report.md">Bug</a> ·
    🚀 <a href="https://github.com/drewmelo/EWA-MarketDynamics/issues/new?labels=enhancement&template=solicitacao_funcionalidade.md">Feature</a>
  </p>

</div>

## Autor

<p align="center">
  <a href="https://drewmelo.github.io/blogr/">
    <img src="https://avatars.githubusercontent.com/u/143213346?s=400&v=4" width="110" style="border-radius:50%">
    <br/>
    <sub><b>André V. P. de Melo</b></sub>
  </a>
</p>

### Stack principal

- **Modelagem e simulação**
  - `rgamer`: implementação dos modelos *Experience-Weighted Attraction* (EWA), *Reinforcement Learning* (RL) e *Belief-based Learning* (BL) 

- **Manipulação de dados**
  - `dplyr`, `tidyr`, `purrr`, `tibble`  

- **Visualização**
  - `ggplot2`, `patchwork`, `scales`  

- **Aplicação interativa**
  - Shiny: interface para exploração das simulações  

- **Documentação**
  - Quarto: versão digital do TCC  

- **Reprodutibilidade**
  - `renv`  

Além disso, o projeto utiliza GitHub Actions para rodar testes leves que verificam funções centrais com base em dados simulados, o que garante a consistência dos resultados sem depender da execução completa das simulações.

## Sobre este repositório

Este repositório concentra o código-fonte do meu TCC sobre **dinâmicas de aprendizado em cenários de incerteza de mercado**. Investigo três algoritmos de aprendizagem em jogos 2×2, EWA, RL e BL, e comparo a trajetória dos agentes com os **equilíbrios teóricos**. O foco recai sobre dois ambientes clássicos: **Buyer–Seller Game (BSG)** e **Market Entry Game (MEG)**.

> **Compatibilidade:**  
> Este projeto foi desenvolvido em **R 4.3.2** e também testado em **R 4.5.1**.  
> Para reprodutibilidade completa, recomenda-se restaurar o ambiente com **`renv::restore()`** antes de executar os scripts.

## O que você encontrará aqui

- **Modelos em forma normal** para BSG e MEG, implementados com `rgamer`, incluindo definição de jogadores, estratégias e payoffs.  
- **Simulador baseado em agentes** com execução de rodadas sob EWA, RL e BL, controle de parâmetros e de horizontes (amostras e períodos). A metodologia descreve as equações do EWA e seus casos especiais (RL e BL), além da implementação em R.  
- **Exploração interativa via Shiny** para ajustar $\lambda$ (sensibilidade à atração) e observar a evolução das proporções de escolha por estratégia e por jogador ao longo do tempo.
- **Geração de tabelas e figuras** (métricas descritivas, distribuições, correlações e matrizes de jogo) prontas para exportação em **PDF**, **PNG** e **LaTeX**.

## Metodologia (resumo)

As matrizes de BSG e MEG são definidas via `rgamer::normal_form`, com payoffs especificados por funções (BSG) ou vetores (MEG). A infraestrutura de simulação cria amostras e períodos, agrega os resultados e produz visualizações por jogador e estratégia.  
Os parâmetros do EWA seguem faixas e relações comuns na literatura (por exemplo, $\phi \approx 0{,}62$, $\delta \approx 0{,}75$, $\rho \le \phi$), com ajustes para jogos 2×2.

## Como replicar o repositório

Abaixo vai um guia direto para reproduzir as simulações, figuras e tabelas (e para rodar o app Shiny localmente).

### 1) Pré-requisitos

- **R ≥ 4.3**
- (Opcional) RStudio/Positron
- **Sistema:**  
  - Windows: instale o *Rtools* correspondente à versão do R.  
  - Linux: garanta as libs de gráficos para PDF (ex.: `libcairo2`, `libharfbuzz`, `libfribidi`).
  - macOS: *Xcode Command Line Tools* já resolve o essencial.

> Se o `cairo_pdf` não estiver disponível no seu sistema, troque `device = cairo_pdf` por `device = "pdf"` nas chamadas `ggsave(...)` ou instale as libs de Cairo.

### 2) Clonar o projeto

```bash
git clone https://github.com/drewmelo/EWA-MarketDynamics.git
cd EWA-MarketDynamics
```

## Reprodutibilidade

Este projeto usa `renv` para congelar as versões dos pacotes de R e permitir que qualquer pessoa recrie exatamente o mesmo ambiente.

### Como reproduzir
```r
# dentro do R
install.packages("renv")   # só na 1ª vez
renv::restore()            # recria o MESMO ambiente do projeto
targets::tar_make()        # roda o pipeline (executa main.R e gera saídas)
```

O `targets` detecta automaticamente o que precisa ser atualizado e garante a execução reprodutível de todas as etapas. Além disso, o comando executa o pipeline descrito em `_targets.R`, que por sua vez roda o script principal e gera todos os arquivos de saída (PDFs, PNGs e TEX) automaticamente.

## Painel Interativo (Shiny Dashboard)

O projeto também inclui um painel interativo em Shiny desenvolvido com o tema *bslib* e elementos visuais otimizados para análise de aprendizado em jogos.

> **Para quem é:**  
> O painel foi pensado para **usuários low-code**, que desejam realizar as simulações e gerar todos os resultados do TCC automaticamente, **sem precisar escrever código em R**.

Ele permite:

- Escolher parâmetros de simulação ($\lambda$, número de amostras e períodos);
- Visualizar em tempo real a evolução das estratégias nos jogos BSG e MEG;
- Alternar entre os modelos de aprendizado (EWA, *Reinforcement Learning*, *Belief-based Learning*);
- Baixar simulações completas e resultados agregados em formato CSV;
- Gerar automaticamente as mesmas tabelas e figuras utilizadas no TCC.

<p align="center">
  <img src="https://raw.githubusercontent.com/drewmelo/EWA-MarketDynamics/refs/heads/master/assets/painel.png"
       alt="Painel Shiny: EWA Market Dynamics Dashboard"
       width="850">
  <br>
  <em>EWA Market Dynamics Dashboard: ambiente interativo para simulação e análise</em>
</p>

>  **Dica:**  
> Ao clicar em “Baixar Simulação”, o painel executa o mesmo pipeline automatizado usado no projeto (`targets::tar_make()`), exportando os resultados diretamente, sem necessidade de rodar scripts manualmente.

Caso queira executar o web app:
```r
shiny::runApp("app")  # ou source("app/app.R")
```

 *Assim, qualquer pessoa pode restaurar o mesmo ambiente e reproduzir integralmente todas as análises e resultados do TCC com apenas dois comandos (`renv::restore()` e `targets::tar_make()`) ou via `shiny::runApp()`*