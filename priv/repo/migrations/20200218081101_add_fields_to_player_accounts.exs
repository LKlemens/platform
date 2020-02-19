defmodule Platform.Repo.Migrations.AddFieldsToPlayerAccounts do
  use Ecto.Migration

  def change do
    alter table(:players) do
      add(:display_name, :string)
      add(:password_digest, :string)
    end

    create(unique_index(:players, [:display_name]))
  end
end
