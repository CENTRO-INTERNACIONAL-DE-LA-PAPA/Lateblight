document.addEventListener("DOMContentLoaded", () => {
  console.debug("app2.js loaded");

  // Copy coordinates via event delegation
  document.body.addEventListener("click", (e) => {
    const target = e.target.closest(".copy-coords");
    if (!target) return;
    const text = target.innerText.trim();
    const hint = target.querySelector(".overlay-hint");
    navigator.clipboard?.writeText(text).catch(() => {});
    target.classList.add("copied");
    if (hint) {
      const prev = hint.textContent;
      hint.textContent = "Copied!";
      setTimeout(() => (hint.textContent = prev), 900);
    }
    setTimeout(() => target.classList.remove("copied"), 700);
  });

  // Geolocate via browser API and send to Shiny
  document.body.addEventListener("click", (e) => {
    const btn = e.target.closest("#locate_me");
    if (!btn) return;
    if (!navigator.geolocation) {
      Shiny.setInputValue("geolocate", null, { priority: "event" });
      return;
    }
    navigator.geolocation.getCurrentPosition(
      (pos) => {
        const coords = {
          lat: pos.coords.latitude,
          lon: pos.coords.longitude,
          nonce: Date.now(),
        };
        Shiny.setInputValue("geolocate", coords, { priority: "event" });
      },
      (err) => {
        console.warn("Geolocation error", err);
        Shiny.setInputValue("geolocate", null, { priority: "event" });
      },
    );
  });

  // Auto-scroll chat to latest message
  const attachChatObserver = () => {
    const chatMessages = document.querySelector(".bslib-chat-messages");
    if (!chatMessages) return;
    const observer = new MutationObserver(() => {
      chatMessages.scrollTop = chatMessages.scrollHeight;
    });
    observer.observe(chatMessages, { childList: true, subtree: true });
  };

  attachChatObserver();

  // Re-attach on Shiny re-render events
  document.addEventListener("shiny:value", () => {
    attachChatObserver();
  });
});
