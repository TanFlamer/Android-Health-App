package com.example.myapp.fragmentsSport.listSport;

import android.app.Dialog;
import android.content.Context;
import android.content.Intent;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.view.Window;
import android.widget.ArrayAdapter;
import android.widget.ImageView;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.app.AlertDialog;

import com.example.myapp.R;
import com.example.myapp.mainActivities.Account;
import com.example.myapp.subActivities.DataSport;

import java.util.List;

public class SportListAdapter extends ArrayAdapter<SportListItem> {

    public SportListAdapter(@NonNull Context context, int resource, List<SportListItem> sportListItemList) {
        super(context, resource, sportListItemList);
    }

    @NonNull
    @Override
    public View getView(int position, @Nullable View convertView, @NonNull ViewGroup parent) {
        View currentItemView = convertView;

        if(currentItemView == null)
            currentItemView = LayoutInflater.from(getContext()).inflate(R.layout.sport_list_item, parent, false);

        SportListItem sportListItem = getItem(position);

        TextView typeView = currentItemView.findViewById(R.id.sportType);
        TextView energyView = currentItemView.findViewById(R.id.sportEnergy);

        typeView.setText(sportListItem.getType());
        energyView.setText(String.valueOf(sportListItem.getCaloriePerMinute()));

        ImageView clickEdit = currentItemView.findViewById(R.id.clickEdit);
        clickEdit.setOnClickListener(v -> {
            Dialog dialog = new Dialog(getContext());
            dialog.requestWindowFeature(Window.FEATURE_NO_TITLE);
            dialog.setCancelable(true);
            dialog.setContentView(R.layout.dialog_sport);
            dialog.show();
        });

        ImageView clickDelete = currentItemView.findViewById(R.id.clickDelete);
        clickDelete.setOnClickListener(view -> new AlertDialog.Builder(getContext())
                .setTitle("Delete Item")
                .setMessage("Are you sure you want to delete this item?")
                .setPositiveButton("Yes", null)
                .setNegativeButton("No", null)
                .create()
                .show());

        return currentItemView;
    }
}
