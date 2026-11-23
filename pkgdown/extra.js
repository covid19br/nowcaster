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
