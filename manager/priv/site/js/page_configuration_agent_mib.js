$(document).ready(function()
{
	$(".mibTest").click(mib_test_click);
	window.ajaxContainer = {}; // store ajax request
});

function mib_test_click(event)
{
	var $mib_test_result = $(event.target).parent().parent().find(".mibTestResult");
	var id = $(event.target).parent().parent().find("[name='id']").val();
	
	$mib_test_result.text("");

	var opts = {
		lines: 10, // The number of lines to draw
		length: 3, // The length of each line
		width: 2, // The line thickness
		radius: 4, // The radius of the inner circle
		corners: 1, // Corner roundness (0..1)
		rotate: 0, // The rotation offset
		color: '#000', // #rgb or #rrggbb
		speed: 1, // Rounds per second
		trail: 60, // Afterglow percentage
		shadow: false, // Whether to render a shadow
		hwaccel: false, // Whether to use hardware acceleration
		className: 'spinner', // The CSS class to assign to the spinner
		zIndex: 2e9, // The z-index (defaults to 2000000000)
		top: -8, // Top position relative to parent in px
		left: 7 // Left position relative to parent in px
	};
	var spinner = new Spinner(opts).spin($mib_test_result[0]);

	if(window.ajaxContainer[id])
	{
		window.ajaxContainer[id].abort(); 
	}
	window.ajaxContainer[id] = $.ajax({
		url: "/configuration/agent_mib/test",
		type: "POST",
		data: { id: id },
		success: function(data, textStatus, jqXHR)
		{
			if(data.success)
			{
				$mib_test_result.text(data.data.value);
			}
			else
			{
				$mib_test_result.html("<label style=\"color: Red\">(failed)</label>");
			}
		},
		error: function(jqXHR, textStatus, errorThrown)
		{
			if(textStatus !== "abort")
			{
				$mib_test_result.html("<label style=\"color: Red\">(failed)</label>");
			}
		},
		complete: function(jqXHR, textStatus)
		{
			spinner.stop();		
		}
	});

	return false;
}


