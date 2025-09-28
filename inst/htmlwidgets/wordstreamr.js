HTMLWidgets.widget({

  name: 'wordstreamr',
  type: 'output',

  factory: function(el, width, height) {

    // Variável para armazenar a última chamada de dados/configurações para redimensionamento
    var last_x;

    // Função de renderização separada para ser chamada na primeira vez e no redimensionamento
    function render(x, width, height) {
      // Limpa o conteúdo anterior
      el.innerHTML = "";

      // A biblioteca wordstream.js espera uma seleção D3 de um elemento SVG.
      // Então, criamos um SVG dentro do nosso elemento 'el'.
      var svg = d3.select(el).append("svg")
        .attr("width", width)
        .attr("height", height);

      // Mapeia as configurações do R (x.settings) para o objeto 'config'
      // que a biblioteca JS espera.
      var config = {
        minFont: x.settings.minFontSize,
        maxFont: x.settings.maxFontSize,
        topWord: x.settings.topWords,
        curve: d3.curveMonotoneX, // Um padrão razoável, pode ser exposto como opção no R
        tickFont: 12, // Pode ser exposto como opção
        legendFont: 14 // Pode ser exposto como opção
      };

      // A biblioteca espera que os dados já estejam no formato correto,
      // com as categorias (POS tags) como chaves. A função em R fará essa transformação.
      // Se não houver dados, não faz nada.
      if (x.data === null || x.data.length === 0) {
        return;
      }

      // Chama a função da biblioteca com os argumentos corretos.
      window.wordstream(svg, x.data, config);
    }

    return {
      renderValue: function(x) {
        // Armazena os dados e configurações para uso posterior no redimensionamento
        last_x = x;
        // Chama a função de renderização com as dimensões atuais do elemento
        render(x, el.clientWidth, el.clientHeight);
      },

      resize: function(width, height) {
        // Se já houve uma renderização, renderiza novamente com as novas dimensões
        if (last_x) {
          render(last_x, width, height);
        }
      }
    };
  }
});
