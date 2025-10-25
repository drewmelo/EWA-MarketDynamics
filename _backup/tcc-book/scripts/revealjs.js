document.addEventListener('DOMContentLoaded', () => {
  // 1) Se a página atual é 'preface', remova número do título
  if (document.body.classList.contains('preface')) {
    document.querySelectorAll('#title-block-header .chapter-number, .quarto-title .chapter-number')
      .forEach(el => el && el.remove());
  }

  // 2) Remova número na sidebar para index/epigrafe (links para as páginas)
  const targets = ['index.html', 'epigrafe.html'];
  const sideLinks = document.querySelectorAll('#quarto-sidebar a, .sidebar a');
  sideLinks.forEach(a => {
    const href = a.getAttribute('href') || '';
    if (targets.some(t => href.endsWith(t) || href.includes(`/${t}`))) {
      const num = a.querySelector('.chapter-number');
      if (num) num.remove();
      // ajusta título para não “sangrar” espaço do número
      const title = a.querySelector('.chapter-title');
      if (title) title.style.marginLeft = '0';
    }
  });
});
