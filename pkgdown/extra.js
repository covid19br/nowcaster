// Language toggle: send the reader to the same page in the other language.
// The English site lives at <root>/ and the Portuguese one at <root>/pt/; both
// have the same page structure. The toggle link (see navbar.components.lang in
// _pkgdown.yml and pkgdown/pt/_pkgdown.yml) points at the other site's root,
// and this script appends the path of the current page to it.
document.addEventListener("DOMContentLoaded", function () {
  var link = document.querySelector(
    'a[aria-label="Ver o site em português"], a[aria-label="View the site in English"]'
  );
  if (!link) return;

  var toPortuguese = /\/pt\/?$/.test(link.href);
  var enRoot = toPortuguese ? link.href.replace(/pt\/?$/, "") : link.href;
  if (!/\/$/.test(enRoot)) enRoot += "/";
  var ptRoot = enRoot + "pt/";

  var here = window.location.href.split("#")[0].split("?")[0];
  var page = null;
  if (toPortuguese && here.indexOf(enRoot) === 0) {
    page = here.slice(enRoot.length);
  } else if (!toPortuguese && here.indexOf(ptRoot) === 0) {
    page = here.slice(ptRoot.length);
  }
  if (page !== null) {
    link.href = (toPortuguese ? ptRoot : enRoot) + page + window.location.hash;
  }
});

// Synchronised tabs: articles that show the same example for nowcasting_inla()
// and nowcasting_mgcv() use one tabset per code block. Choosing a tab (e.g.
// "mgcv") switches every tabset on the page to the tab with the same label.
document.addEventListener("DOMContentLoaded", function () {
  if (typeof bootstrap === "undefined") return;
  var syncing = false;
  var tabs = document.querySelectorAll('button[data-bs-toggle="tab"]');
  tabs.forEach(function (tab) {
    tab.addEventListener("shown.bs.tab", function (event) {
      if (syncing) return;
      syncing = true;
      var label = event.target.textContent.trim();
      tabs.forEach(function (other) {
        if (other !== event.target && other.textContent.trim() === label &&
            !other.classList.contains("active")) {
          bootstrap.Tab.getOrCreateInstance(other).show();
        }
      });
      syncing = false;
    });
  });
});

// Show / hide code: articles get a button above each code block, plus one
// below the title for all the blocks of the page. Code is shown by default;
// the output of each chunk is a separate block and stays visible.
document.addEventListener("DOMContentLoaded", function () {
  if (!/\/articles\/[^\/]+\.html$/.test(window.location.pathname) ||
      /\/articles\/index\.html$/.test(window.location.pathname)) return;
  var main = document.querySelector("main") || document.body;
  var blocks = Array.prototype.slice.call(main.querySelectorAll("div.sourceCode"));
  if (blocks.length === 0) return;

  var pt = (document.documentElement.lang || "").indexOf("pt") === 0;
  var txt = pt ?
    { hide: "Ocultar código", show: "Mostrar código",
      hideAll: "Ocultar todo o código", showAll: "Mostrar todo o código" } :
    { hide: "Hide code", show: "Show code",
      hideAll: "Hide all code", showAll: "Show all code" };

  function setFolded(block, folded) {
    block.classList.toggle("code-folded", folded);
    block.codeFoldButton.textContent = folded ? txt.show : txt.hide;
    block.codeFoldButton.setAttribute("aria-expanded", String(!folded));
  }

  blocks.forEach(function (block) {
    var button = document.createElement("button");
    button.type = "button";
    button.className = "btn btn-sm btn-outline-secondary code-fold-button";
    button.addEventListener("click", function () {
      setFolded(block, !block.classList.contains("code-folded"));
    });
    block.codeFoldButton = button;
    block.parentNode.insertBefore(button, block);
    setFolded(block, false);
  });

  var all = document.createElement("button");
  all.type = "button";
  all.className = "btn btn-sm btn-outline-secondary code-fold-all";
  all.textContent = txt.hideAll;
  all.addEventListener("click", function () {
    var fold = all.textContent === txt.hideAll;
    blocks.forEach(function (block) { setFolded(block, fold); });
    all.textContent = fold ? txt.showAll : txt.hideAll;
  });
  var header = main.querySelector(".page-header") || main.querySelector("h1");
  if (header) header.parentNode.insertBefore(all, header.nextSibling);
});
