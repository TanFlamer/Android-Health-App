package com.example.myapp.fragmentsSport;

import android.app.Dialog;
import android.content.Context;
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
import com.example.myapp.databaseFiles.entity.Type;

import java.util.List;

public class SportListAdapter extends ArrayAdapter<Type> {

    private List<Type> typeList;

    public SportListAdapter(@NonNull Context context, int resource, List<Type> typeList) {
        super(context, resource, typeList);
        this.typeList = typeList;
    }

    @NonNull
    @Override
    public View getView(int position, @Nullable View convertView, @NonNull ViewGroup parent) {
        View currentItemView = convertView;

        if(currentItemView == null)
            currentItemView = LayoutInflater.from(getContext()).inflate(R.layout.sport_list_item, parent, false);

        Type type = typeList.get(position);
        initialiseData(currentItemView, type);
        initialiseEditButton(currentItemView);
        initialiseDeleteButton(currentItemView);
        return currentItemView;
    }

    public void initialiseData(View currentItemView, Type type){
        TextView typeView = currentItemView.findViewById(R.id.sportType);
        TextView energyView = currentItemView.findViewById(R.id.sportEnergy);

        typeView.setText(type.getName());
        energyView.setText(String.valueOf(type.getCaloriePerMinute()));
    }

    public void initialiseEditButton(View currentItemView){
        ImageView clickEdit = currentItemView.findViewById(R.id.clickEdit);
        clickEdit.setOnClickListener(v -> {
            Dialog dialog = new Dialog(getContext());
            dialog.requestWindowFeature(Window.FEATURE_NO_TITLE);
            dialog.setCancelable(true);
            dialog.setContentView(R.layout.data_type);
            dialog.show();
        });
    }

    public void initialiseDeleteButton(View currentItemView){
        ImageView clickDelete = currentItemView.findViewById(R.id.clickDelete);
        clickDelete.setOnClickListener(view -> new AlertDialog.Builder(getContext())
                .setTitle("Delete Item")
                .setMessage("Are you sure you want to delete this item?")
                .setPositiveButton("Yes", null)
                .setNegativeButton("No", null)
                .create()
                .show());
    }

    public void updateTypeList(List<Type> newTypeList){
        typeList.clear();
        typeList.addAll(newTypeList);
        notifyDataSetChanged();
    }
}
