
<div align="center">

  <p align="center">
    <img src="https://raw.githubusercontent.com/drewmelo/EWA-MarketDynamics/refs/heads/master/assets/logo-main.png" alt="Logo" width="120">
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

## Como executar rapidamente

1. Instale **R** (e **RStudio/Positron**, opcional).  
2. Instale as dependências (veja `scripts/pacotes.R`).  
3. Para **gerar figuras e tabelas**: execute `main.R`.  
4. Para **usar o painel interativo**: rode `app/app.R` e abra o Shiny no navegador  
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
cd EWA-MarketDynamics
```

## Reprodutibilidade

Este projeto usa [`renv`](https://rstudio.github.io/renv/) para **congelar** as versões dos pacotes de R e permitir que qualquer pessoa recrie exatamente o mesmo ambiente.

### Como reproduzir

```r
# dentro do R
install.packages("renv")   # só na 1ª vez
renv::restore()            # recria o MESMO ambiente do projeto
targets::tar_make()        # roda o pipeline (executa main.R e gera saídas)
```

O `targets` detecta automaticamente o que precisa ser atualizado e garante a execução reprodutível de todas as etapas. Além disso, o comando executa o pipeline descrito em `_targets.R`, que por sua vez roda o script principal e gera todos os arquivos de saída (PDFs, PNGs e TEX) automaticamente.

## Painel Interativo (Shiny Dashboard)

O projeto também inclui um **painel interativo em Shiny** desenvolvido com o tema **bslib** e elementos visuais otimizados para análise de aprendizado em jogos.

> 🎯 **Para quem é:**  
> O painel foi pensado para **usuários low-code**, que desejam **realizar as simulações e gerar todos os resultados do TCC automaticamente**, **sem precisar escrever código em R**.

Ele permite:

- Escolher parâmetros de simulação (`λ`, número de amostras e períodos);
- Visualizar em tempo real a evolução das estratégias nos jogos **BSG** e **MEG**;
- Alternar entre os modelos de aprendizado (**EWA**, **Reinforcement Learning**, **Belief-based Learning**);
- Baixar **simulações completas** e **resultados agregados** em formato CSV;
- Gerar automaticamente as mesmas tabelas e figuras utilizadas no TCC.

<p align="center">
  <img src="https://raw.githubusercontent.com/drewmelo/EWA-MarketDynamics/refs/heads/master/assets/painel.png"
       alt="Painel Shiny — EWA Market Dynamics Dashboard"
       width="850">
  <br>
  <em>EWA Market Dynamics Dashboard — ambiente interativo para simulação e análise</em>
</p>

>  **Dica:**  
> Ao clicar em “Baixar Simulação”, o painel executa o mesmo pipeline automatizado usado no projeto (`targets::tar_make()`), exportando os resultados diretamente, sem necessidade de rodar scripts manualmente.

Caso queira executar o web app:
```r
shiny::runApp("app")  # ou source("app/app.R")
```

 *Assim, qualquer pessoa pode restaurar o mesmo ambiente e reproduzir integralmente todas as análises e resultados do TCC com apenas dois comandos (`renv::restore()` e `targets::tar_make()`) ou via `shiny::runApp()`*
