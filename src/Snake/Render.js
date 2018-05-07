exports.setScore = function (score) {
  return function() {
    var scoreText = "Score : ";
    scoreText += ("00" + score).slice(-3);
    document.getElementById("score").innerHTML = scoreText;
  };
};
