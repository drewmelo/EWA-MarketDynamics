
<div align="center">

  <p align="center">
    <img src="assets/logo-main.png?raw=1" alt="Logo" width="120">
  </p>

  <h1 align="center">
    Dinâmicas de Aprendizado em Cenários de Incertezas de Mercado: uma
    aplicação de teoria dos jogos com Experience-Weighted Attraction
  </h1>

  <p align="center">
    Trabalho de Conclusão de Curso defendido em: data <br />
    <a href="https://github.com/othneildrew/Best-README-Template"><strong>Explore
    o documento em »</strong></a> <br /><br />
    <a href="https://github.com/othneildrew/Best-README-Template">Veja o artigo sobre este projeto</a> ·
    <a href="https://github.com/othneildrew/Best-README-Template/issues/new?labels=bug&template=bug-report---.md">Reportar bug</a> ·
    <a href="https://github.com/othneildrew/Best-README-Template/issues/new?labels=enhancement&template=feature-request---.md">Solicitar feature</a>
  </p>

</div>


## Autor

<div align="center">

<a href="https://medium.com/@andremelopix">
<img src="https://avatars.githubusercontent.com/u/143213346?s=400&u=958912bd274eeaba08ce1ee6ba79ef60e701992a&v=4" width="115">
<br> <sub><b>André V. P. de Melo</b></sub> </a>

</div>

## Tecnologias utilizadas

<p align="left">
  <img src="https://download.logo.wine/logo/R_(programming_language)/R_(programming_language)-Logo.wine.png" width="100"/>
</p>

O projeto utiliza **R** como base, com ênfase em **Shiny**, **ggplot2**, **dplyr**, **bslib** e **rgamer** para modelagem de jogos, simulação e visualização.

## Sobre este repositório

Este repositório concentra o código-fonte do meu TCC sobre **dinâmicas de aprendizado em cenários de incerteza de mercado**. Investigo três algoritmos de aprendizagem em jogos 2×2 — *Experience-Weighted Attraction* (EWA), *Reinforcement Learning* (RL) e *Belief-based Learning* (BL) — e comparo a trajetória dos agentes com os **equilíbrios teóricos**. O foco recai sobre dois ambientes clássicos: **Buyer–Seller Game (BSG)** e **Market Entry Game (MEG)**.

## O que você encontrará aqui

- **Modelos em forma normal** para BSG e MEG, implementados com `rgamer`, incluindo definição de jogadores, estratégias e payoffs.  
- **Simulador baseado em agentes** com execução de rodadas sob EWA, RL e BL, controle de parâmetros e de horizontes (amostras e períodos). A metodologia descreve as equações do EWA e seus casos especiais (RL e BL), além da implementação em R.  
- **Exploração interativa via Shiny** para ajustar $\lambda$ (sensibilidade à atração) e observar a evolução das proporções de escolha por estratégia e por jogador ao longo do tempo.

<p align="center">
  <img src="assets/painel.png?raw=1"
       alt="Painel Shiny — Simulações dos jogos BSG e MEG"
       width="850">
  <br>
  <em>Painel Shiny — Simulações dos jogos BSG e MEG</em>
</p>

- **Geração de tabelas e figuras** (métricas descritivas, distribuições, correlações e matrizes de jogo) prontas para exportação em **PDF**, **PNG** e **LaTeX**.

## Metodologia (resumo)

As matrizes de BSG e MEG são definidas via `rgamer::normal_form`, com payoffs especificados por funções (BSG) ou vetores (MEG). A infraestrutura de simulação cria amostras e períodos, agrega os resultados e produz visualizações por jogador e estratégia.  
Os parâmetros do EWA seguem faixas e relações comuns na literatura (por exemplo, $\phi \approx 0{,}62$, $\delta \approx 0{,}75$, $\rho \le \phi$), com ajustes para jogos 2×2.

## Como executar rapidamente

1. Instale **R** (e **RStudio/Positron**, opcional).  
2. Instale as dependências (veja `scripts/pacotes.R`).  
3. Para **gerar figuras e tabelas**: execute `main.R`.  
4. Para **usar o painel interativo**: rode `app.R` e abra o Shiny no navegador  
   (ou acesse: <https://drewmelo.shinyapps.io/ewa-market-dynamics/>).

**Observação.** O repositório inclui *helpers* para exportação de figuras (`ggsave`) e tabelas (LaTeX/PNG), além de rotinas para varrer valores de $\lambda$ e comparar as curvas de aprendizado entre modelos.

## Como replicar o repositório

Abaixo vai um guia direto para reproduzir as simulações, figuras e tabelas (e para rodar o app Shiny localmente).

### 1) Pré-requisitos

- **R ≥ 4.3**
- (Opcional) **RStudio/Positron**
- **Sistema:**  
  - **Windows:** instale o **Rtools** correspondente à versão do R.  
  - **Linux:** garanta as libs de gráficos para PDF (ex.: `libcairo2`, `libharfbuzz`, `libfribidi`).
  - **macOS:** *Xcode Command Line Tools* já resolve o essencial.

> Se o `cairo_pdf` não estiver disponível no seu sistema, troque `device = cairo_pdf` por `device = "pdf"` nas chamadas `ggsave(...)` ou instale as libs de Cairo.

### 2) Clonar o projeto

```bash
git clone https://github.com/drewmelo/EWA-MarketDynamics.git
cd EWA-MarketDynamics.git
```