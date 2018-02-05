var sidebarElement = document.getElementById('sidebar');
var navElement = document.getElementById('nav');
var navToggle = document.getElementById('nav-toggle');

var navHidden = false;
function updateNavVisibility() {
  if (navHidden) {
    navElement.style.maxHeight = 0;
    navElement.style.padding = 0;
  } else {
    navElement.style.maxHeight = null;
    navElement.style.padding = null;
  }
  if (toggleVisible) {
    sidebarElement.style.maxHeight = 'none';
  } else {
    sidebarElement.style.maxHeight = null;
  }
}

var toggleVisible = false;
function checkToggleVisibility() {
  var newToggleVisible = (navToggle.offsetParent !== null);
  if (newToggleVisible && !toggleVisible) {
    navHidden = true; // whenever the toggle appears, hide the nav...
  }
  if (!newToggleVisible && toggleVisible) {
    navHidden = false; // ...and whenever it disappears, show the nav.
  }
  toggleVisible = newToggleVisible;
  updateNavVisibility();
}

navToggle.addEventListener('click', function () {
  navHidden = !navHidden;
  updateNavVisibility();
});

window.addEventListener('resize', checkToggleVisibility);
checkToggleVisibility();
