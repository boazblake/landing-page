defmodule LandingPageWeb.V1.LeadControllerTest do
  use LandingPageWeb.ConnCase

  describe "POST /api/v1/leads" do
    test "returns error response with invalid params", %{conn: conn} do
      conn = post(conn, lead_path(conn, :create), %{"lead" => %{}})

      assert json_response(conn, 422) == %{
                "full_name" => ["can't be blank"],
                "email" => ["can't be blank"]
              }
    end

    test "returns success response with valid params", %{conn: conn} do
      params = %{
        "lead" => %{
          "full_name" => "Boaz Blake",
          "email" => "boaz@boaz.com"
        }
      }

      conn = post(conn, lead_path(conn, :create), params)
      assert json_response(conn, 200) == params["lead"]
    end
  end
end