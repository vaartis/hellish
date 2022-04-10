{
    const ELEMENT_ID = "replies-total",
          STORAGE_KEY = "seenReplyCounts";

    let repliesTotalBlock = document.getElementById(ELEMENT_ID);
    let repliesNumber = Number(repliesTotalBlock.innerHTML);

    let replyCountName
    let replyCounts = localStorage.getItem(STORAGE_KEY);
    if (replyCounts) {
        replyCounts = JSON.parse(replyCounts);
    } else {
        replyCounts = {};
    }

    replyCounts[window.location.pathname] = repliesNumber;
    localStorage.setItem(STORAGE_KEY, JSON.stringify(replyCounts));
}
